Load Testing
------------

You can find a working version of the pronk load testing tool at
https://github.com/fhartwig/pronk/tree/barely-working-state
Example usage:
pronk "http://localhost:7002/login" -n 10000 -c 300
where 10000 is the total number of requests and 300 is the number of concurrent
requests.
