# HView Library

## Overview

The HView library provides a WebSocket server implemented in Haskell using the `websockets` library, along with a JavaScript client for real-time updates of HTML elements. Inspired by LiveView, the server sends JSON messages to clients, allowing for dynamic updates of specific HTML elements on the client-side.

This project is currently in proto-alpha (προτο-α) phase.

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

This will start the server, which will listen for WebSocket connections on `ws://127.0.0.1:3001` and serves an example app on `http://localhost:3000/page`.


