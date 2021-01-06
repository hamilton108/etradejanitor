import pika


def connection():
    credentials = pika.PlainCredentials("etradejanitor", "VhCHeUJ40")

    parameters = pika.ConnectionParameters("172.20.1.4",
                                           5672,
                                           "etradejanitor_vhost",
                                           credentials)

    return pika.BlockingConnection(parameters)


def channel(conn, exchange_name, **kw):
    # exchange_name = kwargs.get("exchange_name", "topic-logs")
    # queue_name = kwargs.get("queue_name", "demo4")

    result = conn.channel()
    result.exchange_declare(exchange=exchange_name, exchange_type="topic")

    if len(kw) > 0:
        queue_name = kw["queue_name"]
        routing_key = kw["routing_key"]
        result.queue_declare(queue=queue_name)
        result.queue_bind(exchange=exchange_name,
                          queue=queue_name,
                          routing_key=routing_key)
    return result


def exchange_declare(channel, exchange_name, exchange_type="topic"):
    channel.exchange_declare(exchange=exchange_name,
                             exchange_type=exchange_type,
                             passive=False,
                             durable=False,
                             auto_delete=False,
                             internal=False,
                             arguments=None)
