# DFA Editor

Editor web para definir, visualizar y simular Autómatas Finitos Deterministas (DFA). El usuario escribe la definición del autómata en un lenguaje de dominio específico (DSL), y el sistema la tokeniza, parsea, genera un diagrama y valida cadenas.

https://lenguaje-de-automatas.onrender.com/ 
---

## Flujo completo

```
Usuario (index.html)
       │  POST / { input, cadena }
       ▼
web-server.rkt        ← orquestador HTTP
       │
       ├─► lexer.rkt          → lista de tokens
       │
       ├─► parser.rkt         → hash del autómata
       │
       ├─► graph-renderer.rkt → PNG en base64
       │
       └─► validator.rkt      → true / false
```

### 1. `index.html` — Frontend

El usuario interactúa con dos acciones:

- **Compilar y dibujar**: envía el texto del textarea.
- **Validar cadena**: envía el texto del textarea más una cadena a simular.

Ambas acciones hacen un `POST /` con el cuerpo:

```json
{ "input": "<definición del autómata>", "cadena": "<cadena a validar>" }
```

El servidor responde con:

```json
{
  "resultado": "<HTML con tokens coloreados o errores>",
  "imagen":    "<PNG del diagrama en base64, o string vacío si hay error>",
  "valido":    true | false
}
```

El frontend inyecta el HTML en un `<iframe>`, muestra la imagen del diagrama, e indica si la cadena fue aceptada o rechazada.

---

### 2. `web-server.rkt` — Orquestador HTTP

Recibe el POST y coordina el pipeline:

1. Llama a `tokenize-all` (lexer).
2. Si hay error léxico, detiene el pipeline y devuelve el HTML de error.
3. Llama a `rec-des` (parser) con el stream limpio (sin comentarios ni saltos de línea).
4. Si hay errores sintácticos, devuelve el HTML de errores.
5. Si todo es válido, llama a `genera-imagen` (graph-renderer) y a `valida` (validator).
6. Devuelve JSON con los tres campos.

---

### 3. `lexer.rkt` — Analizador léxico

Convierte el texto plano en una lista de tokens usando _maximal munch_: ante varias reglas que hagan match, gana la que consume más caracteres.

Cada token es una lista `(tipo lexema)`. Tipos reconocidos:

| Tipo               | Ejemplo         | Descripción                        |
|--------------------|-----------------|------------------------------------|
| `states`           | `states`        | Palabra clave de estados           |
| `start_state_op`   | `start_state`   | Palabra clave estado inicial       |
| `accept_states_op` | `accept_states` | Palabra clave estados de aceptación|
| `input_alphabet`   | `input_alphabet`| Palabra clave alfabeto             |
| `delta_op`         | `delta`         | Palabra clave tabla de transiciones|
| `state`            | `q0`, `q1`      | Identificador de estado (`q` + dígitos) |
| `ID`               | `a`, `b`, `0`   | Símbolo del alfabeto (un carácter alfanumérico) |
| `colon_op`         | `:`             | Dos puntos                         |
| `comma_op`         | `,`             | Coma                               |
| `comment_op`       | `# texto`       | Comentario (ignorado por el parser)|
| `newline`          | `\n`            | Salto de línea (ignorado por el parser) |
| `EOF`              | —               | Fin de entrada                     |
| `error`            | `@`             | Carácter no reconocido             |

Exporta: `tokenize-all`, `tokens->html`, `error-tokens?`, `clean-token-stream`.

---

### 4. `parser.rkt` — Parser de descenso recursivo

Recibe el stream de tokens limpio y construye un hash de Racket que representa el DFA. Implementa la siguiente gramática:

```
programa      ::= statesDef stateStart statesAccepted alphabet delta EOF

statesDef     ::= "states" ":" state statesPrime
statesPrime   ::= "," state statesPrime | ε

stateStart    ::= "start_state" ":" state

statesAccepted::= "accept_states" ":" state statesPrime

alphabet      ::= "input_alphabet" ":" ID symbolsPrime
symbolsPrime  ::= "," ID symbolsPrime | ε

delta         ::= "delta" ":" deltafirst
deltafirst    ::= state ":" ID ":" state deltaPrime
deltaPrime    ::= state ":" ID ":" state deltaPrime | ε
```

El autómata resultante es un hash con las claves:
- `"states"` → lista de nombres de estados
- `"iniciales"` → nombre del estado inicial
- `"finales"` → lista de estados de aceptación
- `"alphabet"` → lista de símbolos
- `"<estado>"` → hash de transiciones `{ símbolo → estado_destino }`

Exporta: `rec-des`.

---

### 5. `graph-renderer.rkt` — Generador de diagrama

Recibe el hash del autómata y:

1. Genera un archivo `dfa.dot` en formato Graphviz DOT.
2. Invoca `dot -Tpng dfa.dot -o dfa.png` vía shell.
3. Lee el PNG y lo devuelve como string base64.

Los estados de aceptación se dibujan con `doublecircle`, los demás con `circle`. El estado inicial tiene una flecha desde un nodo invisible `start`.

Exporta: `genera-imagen`.

---

### 6. `validator.rkt` — Simulador del DFA

Recibe el hash del autómata y una cadena. Simula el DFA carácter por carácter desde el estado inicial. Devuelve `#t` si la cadena termina en un estado de aceptación, `#f` en cualquier otro caso (estado no final, transición inexistente, autómata nulo).

Exporta: `valida`.

---

## Sintaxis del DSL

El orden de las secciones es **obligatorio**:

```
states: <estado> [, <estado> ...]
start_state: <estado>
accept_states: <estado> [, <estado> ...]
input_alphabet: <símbolo> [, <símbolo> ...]
delta:
<estado> : <símbolo> : <estado>
[<estado> : <símbolo> : <estado> ...]
```

- Los estados se escriben como `q` seguido de dígitos: `q0`, `q1`, `q12`.
- Los símbolos del alfabeto son caracteres alfanuméricos individuales: `a`, `b`, `0`.
- Los comentarios empiezan con `#` y se extienden hasta el fin de línea.
- El espaciado y los saltos de línea dentro de las secciones son opcionales, excepto que las reglas de delta pueden ir en líneas separadas.

---

## Casos de prueba

### Caso 1 — DFA válido: acepta exactamente "ab"

```
states: q0, q1, q2
start_state: q0
accept_states: q2
input_alphabet: a, b
delta:
q0 : a : q1
q1 : b : q2
```

| Cadena | Resultado  |
|--------|------------|
| `ab`   | ACEPTADA   |
| `a`    | RECHAZADA  |
| `b`    | RECHAZADA  |
| `aba`  | RECHAZADA  |
| ``     | RECHAZADA  |

---

### Caso 2 — DFA válido: acepta cadenas que terminan en "a"

```
# Acepta cadenas sobre {a,b} que terminan en 'a'
states: q0, q1
start_state: q0
accept_states: q1
input_alphabet: a, b
delta:
q0 : a : q1
q0 : b : q0
q1 : a : q1
q1 : b : q0
```

| Cadena  | Resultado  |
|---------|------------|
| `a`     | ACEPTADA   |
| `ba`    | ACEPTADA   |
| `bba`   | ACEPTADA   |
| `b`     | RECHAZADA  |
| `ab`    | RECHAZADA  |
| ``      | RECHAZADA  |

---

### Caso 3 — DFA de un solo estado (acepta cadena vacía)

```
states: q0
start_state: q0
accept_states: q0
input_alphabet: a
delta:
q0 : a : q0
```

| Cadena | Resultado  |
|--------|------------|
| ``     | ACEPTADA   |
| `a`    | ACEPTADA   |
| `aaa`  | ACEPTADA   |

---

### Caso 4 — Error léxico

```
states: q0, q1
start_state: q0
accept_states: q1
input_alphabet: a
delta:
q0 : @ : q1
```

El carácter `@` no es reconocido por el lexer. Se mostrará en el panel de tokens resaltado en rojo con el mensaje:
```
← Error léxico: '@'
```
El pipeline se detiene; no se genera diagrama ni se valida ninguna cadena.

---

### Caso 5 — Error sintáctico

```
states: q0 q1
start_state: q0
accept_states: q1
input_alphabet: a
delta:
q0 : a : q1
```

Falta la coma entre `q0` y `q1` en `states`. El parser reportará:
```
Error: esperaba EOF, se recibio state
```

---

### Caso 6 — Con comentarios

```
# Autómata que acepta la cadena "0"
states: q0, q1
start_state: q0   # estado inicial
accept_states: q1
input_alphabet: 0
delta:
q0 : 0 : q1
```

Los comentarios son ignorados por el parser. El autómata se compila correctamente.

| Cadena | Resultado  |
|--------|------------|
| `0`    | ACEPTADA   |
| `00`   | RECHAZADA  |
| ``     | RECHAZADA  |
