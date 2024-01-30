import json
import random
import string

# Função para gerar uma string aleatória de tamanho especificado
def generate_random_string(size):
    return ''.join(random.choices(string.ascii_letters + string.digits, k=size))

# Tamanho total desejado em bytes (1MB = 1024 * 1024 bytes)
desired_size = 1024 * 100

# Inicializar uma lista para armazenar os dados gerados
data = []

# Loop até que o tamanho total dos dados atinja o tamanho desejado
current_size = 0
while current_size < desired_size:
    # Gerar um dicionário com dados aleatórios
    random_data = {
        "id": random.randint(1, 1000000),
        "name": generate_random_string(20),
        "age": random.randint(18, 80),
        "address": generate_random_string(30),
        "email": generate_random_string(15) + "@example.com"
    }
    
    # Adicionar os dados gerados à lista
    data.append(random_data)
    
    # Converter a lista para JSON e calcular o tamanho atual em bytes
    json_data = json.dumps(data)
    current_size = len(json_data.encode())

# Salvar os dados em um arquivo JSON
with open('dados_aleatorios.json', 'w') as json_file:
    json.dump(data, json_file)

print("Arquivo JSON gerado com sucesso.")
