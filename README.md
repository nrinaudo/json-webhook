# json-webhook

Simple HTTP server meant for debugging JSON webhooks: any JSON data `PUT` on `host/:id` will be stored for 24 hours and retrievable by `GET`ing the same endpoint.
