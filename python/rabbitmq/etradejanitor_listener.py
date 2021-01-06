import pika
import sys
import common


connection = common.connection()

channel = connection.channel()

exchange_name = "etrade"

common.exchange_declare(channel, exchange_name)

result = channel.queue_declare("etrade-logs", exclusive=False)
queue_name = result.method.queue

binding_keys = ["etrade.*"]

"""
binding_keys = sys.argv[1:]
if not binding_keys:
    sys.stderr.write("Usage: %s [binding_key]...\n" % sys.argv[0])
    sys.exit(1)
"""

for binding_key in binding_keys:
    channel.queue_bind(
        exchange=exchange_name, queue=queue_name, routing_key=binding_key)


print(' [*] Waiting for logs. To exit press CTRL+C')


def callback(ch, method, properties, body):
    print(ch)
    print(properties)
    print(" [x] %r:%r" % (method.routing_key, body))


channel.basic_consume(
    queue=queue_name, on_message_callback=callback, auto_ack=True)

channel.start_consuming()
