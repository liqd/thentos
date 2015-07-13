# Load Testing

You can find a working version of the pronk load testing tool at

https://github.com/liqd/pronk/tree/thentos-patches

Example usage:

```bash
pronk "http://localhost:7002/login" -n 10000 -c 300
```

10000 is the total number of requests and 300 is the number of concurrent
requests.

