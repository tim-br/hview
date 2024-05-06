document.addEventListener("DOMContentLoaded", function() {
  console.log("hello lex");

  // Create a new WebSocket connection.
  const socket = new WebSocket('ws://127.0.0.1:3001');

  // Connection opened event handler.
  socket.addEventListener('open', function (event) {
      console.log('Connected to WebSocket server.');
      socket.send('Hello, world!');
  });

  // Listen for messages from the server.
  socket.addEventListener('message', function (event) {
      console.log('Message from server:', event.data);
      const jsonString = event.data

      // Parse JSON string
      const data = JSON.parse(jsonString);
  
      // Extract h_id and html
      const { h_id, html } = data;
  
      // Find the HTML element with the matching h-id attribute
      const element = document.querySelector(`[h-id='${h_id}']`);
  
      // Check if the element exists
      if (element) {
        // Create a new element from the HTML string
        const newElement = document.createRange().createContextualFragment(html);

        // Replace the existing element with the new element
        element.replaceWith(newElement);
        console.log(`Replaced element with h-id='${h_id}'`);
        resetListeners()
      } else {
          // Log if no element was found
          console.log(`No element found with h-id='${h_id}'`);
      }
  });

  // Handle any errors that occur.
  socket.addEventListener('error', function (event) {
      console.error('WebSocket error:', event);
  });

  // Handle WebSocket closure.
  socket.addEventListener('close', function (event) {
      console.log('WebSocket connection closed.');
  });

  resetListeners()

  function resetListeners() {
    const parents = document.querySelectorAll('[h-id]');

    parents.forEach(parent => {
        // Get the 'h-id' value from the parent
        const hIdValue = parent.getAttribute('h-id');
  
        // Find the element within this parent that has the 'h-value' attribute and get its integer value
        const hValueElement = parent.querySelector('[h-value]');
        const hValue = hValueElement ? parseInt(hValueElement.textContent) : 0;
  
        // Find all children within this parent that have 'h-click' attribute
        const children = parent.querySelectorAll('[h-click]');
        
        children.forEach(child => {
            // Extract the 'h-click' attribute value
            const hClickValue = child.getAttribute('h-click');
  
            // Add click event listener to each child element
            child.addEventListener('click', function() {
                // Send the structured data to the WebSocket server
                const dataToSend = { hID: hIdValue, body: {dispatch: hClickValue, payload: hValue }};
                socket.send(JSON.stringify(dataToSend));
                console.log('Data sent to server:', dataToSend);
            });
        });
    });
  
    if (parents.length === 0) {
        console.log('No elements with h-id found.');
    }
  }
});
