#!/usr/bin/python3

import json
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

mdb = common.mongoClient()


def mongoCollection(routing_key):
    if routing_key == "etrade.info":
        return mdb.etrade.loginfo
    elif routing_key == "etrade.error":
        return mdb.etrade.logerror
    else:
        return None


def bytes_to_json(b):
    s = b.decode("utf-8")
    return json.loads(s)


def callback(ch, method, properties, body):
    print(method.routing_key)
    coll = mongoCollection(method.routing_key)
    coll.insert_one(bytes_to_json(body))


def callback_debug(ch, method, properties, body):
    print(method.routing_key)
    print(body)


def consume():
    channel.basic_consume(
        queue=queue_name, on_message_callback=callback, auto_ack=True)
    channel.start_consuming()


"""
def demo():
    bytes = b'{"utcTime":1610387394,"nx":1608246000000,"msg":"Network.Socket.getAddrInfo (called with preferred socket type/protocol: AddrInfo {addrFlags = [AI_ADDRCONFIG], addrFamily = AF_UNSPEC, addrSocketType = Stream, addrProtocol = 0, addrAddress = <assumed to be undefined>, addrCanonName = <assumed to be undefined>}, host name: Just \\"www.nordnet.nox\\", service name: Just \\"443\\"): does not exist (Name or service not known)"}'
    coll = mongoCollection("etrade.error")
    coll.insert_one(bytes_to_json(bytes))
"""


if __name__ == "__main__":
    print(" [*] Waiting for logs. To exit press CTRL+C")
    consume()
