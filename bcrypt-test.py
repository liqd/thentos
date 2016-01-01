import sys
from cryptacular.bcrypt import BCRYPTPasswordManager

manager = BCRYPTPasswordManager()
password = 'wefwefwef'
password_encoded = manager.encode(password)

print(password)
print(password_encoded)
