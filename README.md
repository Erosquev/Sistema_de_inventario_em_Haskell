# Sistema de inventario em Haskell

- **Disciplina:** Programação Lógica e Funcional
- **Instituição:** Pontifícia Universidade Católica do Paraná    
- **Professor:** Frank Coelho de Alcantara  
- **Alunos:**  
  - Eros Felipe de Quevedo dos Santos — [@Erosquev](https://github.com/Erosquev)  
  - Victor Augusto Esmaniotto — [@Vitinnn123](https://github.com/Vitinnn123)
 
---

## Objetivo

A ideia do projeto é desenvolver um sistema de gerenciamento de inventário em Haskell, capaz de registrar, atualizar e remover itens do estoque de forma segura e totalmente persistente. O sistema também mantém um arquivo de auditoria com todas as operações executadas, permitindo identificar erros, acompanhar movimentações e gerar relatórios detalhados sobre o histórico de uso. O objetivo é proporcionar uma solução simples, funcional e confiável, que demonstre boas práticas de programação funcional, manipulação de arquivos e controle de estado.

---

## Tecnologias Utilizadas
- **Linguagem:** Haskell
- **Ambiente de execução:** OnlineGDB
- **Editor utilizado:** OnlineGDB

---

## Estrutura do Projeto

```
/Projeto Sistema de inventario em Haskell
├── main.hs - Código Completo 
└── README.md - Documentação do projeto
```

---

## Como Executar

1. Acesse o **OnlineGDB** (https://www.onlinegdb.com/).  
2. No editor que abrir, apague o código padrão e **cole o código do Sistema de inventario em Haskell**  
3. Em **Language**, localizada no **canto superior direito**, selecione a linguagem **Haskell**
4. Clique no botão **Run** para compilar e iniciar o programa.
5. Assim que o console abrir, você verá o menu principal do sistema.

## Cenário 1: Persistência de Estado (Sucesso):

1. Iniciar o programa (sem arquivos de dados):

2. Adicionar 3 itens: 

**Comando**
```
add P01 Teclado 10 Informatica
add P02 Mouse 5 Informatica
add P03 Monitor 3 Informatica
```
**Resultado**
```
Item adicionado com sucesso!
Item adicionado com sucesso!
Item adicionado com sucesso!
```

3. Fechar o programa:

**Comando**
```
exit
```
**Resultado**
```
Saindo...
```

4. Verificar se os arquivos foram criados:

```
Inventario.dat
Auditoria.log
```
  
5. Reiniciar o programa:

6. Executar comando de listagem:

**Comando**
```
list
```
**Resultado**
```
=== Itens no inventário ===
Item {itemID = "P01", nome = "Teclado", quantidade = 10, categoria = "Informatica"}
Item {itemID = "P02", nome = "Mouse", quantidade = 5, categoria = "Informatica"}
Item {itemID = "P03", nome = "Monitor", quantidade = 3, categoria = "Informatica"}
```

## Cenário 2: Erro de Lógica (Estoque Insuficiente)

1. Adicionar um item com 10 unidades:

**Comando**
```
add T10 Teclado 10 Informatica
```
**Resultado**
```
Item adicionado com sucesso!
```

2. Tentar remover mais do que o estoque (15 unidades):
   
**Comando**
```
remove T10 15
```
**Resultado**
```
Erro: Estoque insuficiente.
```

3. Verificar inventário:

**Comando**
```
list
```
**Resultado**
```
=== Itens no inventário ===
Item {itemID = "T10", nome = "Teclado", quantidade = 10, categoria = "Informatica"}
```

4. Verificar se o log da operação foi gravado no Auditoria.log:

```
LogEntry {timestamp = 2025-11-15 01:07:57.146195553 UTC, acao = Add, detalhes = "Item Teclado adicionado com sucesso.", status = Sucesso}
LogEntry {timestamp = 2025-11-15 01:08:25.816299546 UTC, acao = Remove, detalhes = "Estoque insuficiente.", status = Falha "Estoque insuficiente."}
```

## Cenário 3: Geração de Relatório de Erros

1.Executar o comando de relatório:

**Comando**
```
report
```
**Resultado**
```
================== RELATÓRIO ==================
Total de registros de log: 2
----------------------------------------------
Erros registrados:
Estoque insuficiente.
----------------------------------------------
Item mais movimentado: Estoque (1 ocorrências)
==============================================
```

---

## Possíveis Melhorias Futuras

* Implementar uma interface gráfica ou painel web para substituir o uso exclusivo do terminal, facilitando o gerenciamento do inventário.
* Adicionar autenticação de usuários (admin, operador, auditor) para maior segurança no controle das operações críticas.
* Integrar o inventário com um banco de dados relacional (MySQL), garantindo maior escalabilidade e segurança.
* Criar um sistema de backup automático dos dados e logs, evitando perda de informações em caso de falha.
* Adicionar suporte a categorias hierárquicas e subcategorias, tornando a organização dos itens mais detalhada.
* Implementar filtros avançados na listagem de itens (por categoria, por quantidade, por faixa de estoque).
* Permitir exportação do inventário e dos relatórios em formatos CSV, JSON ou PDF.
* Incluir gráficos no relatório (como itens mais movimentados, itens mais críticos, histórico de alterações).
* Criar um mecanismo de logs criptografados para aumentar a integridade da auditoria.
* Adicionar suporte multilíngue (Português/Inglês) para tornar o sistema mais universal.
* Permitir integração com sistemas externos, como ERPs ou sensores IoT para atualização automática do estoque.
---

## Licença

Este projeto foi desenvolvido **exclusivamente para fins educacionais** na disciplina de *Programação Lógica e Funcional* da Pontifícia Universidade Católica do Paraná.
Não possui finalidade comercial e não concede direitos de uso além do contexto acadêmico.
