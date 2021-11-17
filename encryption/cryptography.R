# HASHING

# Let's build a Simple Hashing functions from Scratch
naivehash <- function(val) sum(utf8ToInt(val)) %% 100 
naivehash("Hello Hashed Message!")
naivehash("Hello Hashed Message 2!")

# Hashing functions (SHA-256, MD5, ...)
# https://en.wikipedia.org/wiki/SHA-2
install.packages("digest")
library(digest)

hashed.message = digest("Say hallo to a better hash!", algo="sha256")
print(paste("The SHA-256 hash is", nchar(hashed.message), "characters long"))
print(paste("Number of possible hashes:", 16^nchar(hashed.message))) # Why? 16: The hash is in a hexadecmal format
# Hash collision? Not likely. Check the last chart in the URL below. We are using a 64*8=
# : https://preshing.com/20110504/hash-collision-probabilities/

#more secure to append a string every time
digest(paste('mypwd', "service_specific_secret_salt"), algo="sha256")

# Encryptions
# ===========

# Symmetric encryption: Caesar (uses the same key for encoding and decoding)
# shifting letters
# ----------------------------

install.packages("caesar")
library(caesar)

coded = caesar("My Sensitive Message", shift=1)
print(paste("Encoded message:", coded))
decoded = caesar(coded, shift=1, decrypt = TRUE)
print(paste("Decoded message:", decoded))


# Asymmetric / Public Key Encryption (PKI) (separate key for encoding and decoding --> generating pair of keys)
# ---------------------
# 
# Key 1 -> Private Key # For decryption
# Key 2 -> Public Key # For encryption
# (RSA: Short messages can be encrypted with Private and decrypted with Public)
# 
# 1. B sends A Key2
# 2. A encrypts the message with B's Key2(Public) and sends it to back to B
# 3. B decrypts it with Key1(Private)

# If you are using A Mac:
# In a terminal: brew install openssl
# If you are using Linux: Make sure openssl-dev is installed

if (Sys.info()["sysname"] == 'Darwin'){
  Sys.setenv(LDFLAGS="-L/usr/local/opt/openssl@1.1/lib",
             CPPFLAGS="-I/usr/local/opt/openssl@1.1/include",
             PKG_CONFIG_PATH="/usr/local/opt/openssl@1.1/lib/pkgconfig",
             LIBRARY_PATH=paste(Sys.getenv("LIBRARY_PATH"),
                                "/usr/local/opt/openssl@1.1/lib",
                                sep=""))
  print('Setting up OSX environment')
}

install.packages("PKI")
library(PKI)

# Create a keypair and save them in PEM format to variables
keypairprovider <- PKI.genRSAkey(bits = 2048L)

#convert to character base
prv.pem <- PKI.save.key(keypairprovider, private=TRUE)
print(prv.pem)

pub.pem <- PKI.save.key(keypairprovider, private=FALSE)
print(pub.pem)

# Extract the Public key from the public key's PEM format
pub.key <- PKI.load.key(pub.pem)
prv.key <- PKI.load.key(prv.pem)

# Encrypt with the public key
bytes.to.encode = charToRaw("Hello, asymmetric encryption!")
encrypted <- PKI.encrypt(bytes.to.encode, pub.key)
print(encrypted)

writeBin(encrypted, 'enc')
e2 = readBin('enc', what=raw(), n=100000)

# Decrypt with the private key
decrypted <- rawToChar(PKI.decrypt(e2, prv.key))
print(decrypted)

# Save the keys to a file, then load them back
write(pub.pem, file="id_rsa.pub") # Save Public key 
write(prv.pem, file="id_rsa")     # Save Private key

# ...
# Load back the keys
pub.pem.loaded <- scan("id_rsa.pub", what='list', sep='\n') # Load
prv.pem.loaded <- scan("id_rsa", what='list', sep='\n') # Load

# Extract the key objects from the PEM file
pub.key.loaded <- PKI.load.key(pub.pem.loaded) 
prv.key.loaded <- PKI.load.key(prv.pem.loaded) 

# Let's encrypt and decrypt, again!
encrypted.again <- PKI.encrypt(charToRaw("Hello, asymmetric encryption, again!"), pub.key.loaded)
decrypted.again <- rawToChar(PKI.decrypt(encrypted.again, prv.key.loaded))
print(decrypted.again)

## Exercise

# 1. Pair up.
# 2. Generate and exchange public keys

# 3. Send one encrypted question to your peer (both of you).
# 4. Send an encrypted answer back to your peer and decrypt the answer. 
# 4. SAVE IT TO DISK 
  # HINT: You will need to read and write using binary files, like described here:
  # https://www.tutorialspoint.com/r/r_binary_files.htm

binarydata = charToRaw('xxx')
writeBin(binarydata, 'binary.data')

x = readBin('binary.data', (), n=10000)
print(x)
