# Download the helper library from https://www.twilio.com/docs/python/install
from twilio.rest import Client

# Your Account Sid and Auth Token from twilio.com/console
# DANGER! This is insecure. See http://twil.io/secure
# account_sid = 'AC7d1ea4eb9f312033d47702203e96f29c'
# auth_token = '759ff7129455a0a46147c9367cef3993'

account_sid = ''
auth_token = ''
client = Client(account_sid, auth_token)

message = client.messages \
                .create(
                     body="A student has requested a team formation in your area. Visit stemLinked: FIRST LEGO League to create your team!",
                     from_='+15612993065',
                     to='18042008481'
                 )

print(message.sid)
