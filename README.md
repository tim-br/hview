# HView Library

## Overview

The HView library provides a WebSocket server implemented in Haskell using the `websockets` library, along with a JavaScript client for real-time updates of HTML elements. Inspired by LiveView, the server sends JSON messages to clients, allowing for dynamic updates of specific HTML elements on the client-side.

This project is currently in proto-alpha (προτο-α) phase.

## Structure

The main files in the project include:

- **Main Haskell Server**: `Main.hs` contains the main logic for the Haskell WebSocket server.
- **WebSocket Logic**: `HViewWebSockets.hs` handles WebSocket connections, message processing, and JSON serialization.
- **Example App**: `FloatExample.hs` Example app showing basic functionality.
- **JavaScript Client**: `static/js/hview.js` contains the JavaScript client code that connects to the WebSocket server and updates HTML elements.
- **Mustache Templates**: Mustache templates for generating HTML are used in the Haskell server. They can be found in the Haskell code.


## Setup and Running

### Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/README/) for building and running Haskell projects.
- A modern web browser that supports WebSocket connections (e.g., Google Chrome, Mozilla Firefox).

### Running the Server
To run the Haskell WebSocket and web server, execute the following command:

```bash
stack build
stack exec hview
```

This will start the server, which will listen for WebSocket connections on `ws://127.0.0.1:3001` and serves the FloatExample app on `http://localhost:3000/page`.


