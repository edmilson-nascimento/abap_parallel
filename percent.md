# CL_ABAP_PARALLEL - Guia Completo sobre Processamento Paralelo

## 1. O que é a classe CL_ABAP_PARALLEL?

A classe CL_ABAP_PARALLEL é usada para iniciar tarefas paralelas em ABAP. Num sistema ABAP, os processos de diálogo destinam-se ao processamento de solicitações de diálogo. Na maioria dos casos, um grande número de processos de diálogo não é utilizado e podes atribuir esses processos livres ao processamento paralelo.

A classe existe há algum tempo e também é usada no ABAP Cloud. Fornece dois cenários para processamento paralelo de operações intensivas em dados.

**Referência SAP:** https://help.sap.com/docs/btp/sap-business-technology-platform/parallel-processing

---

## 2. Parâmetros do Constructor

Ao criar uma instância de `CL_ABAP_PARALLEL`, podes especificar:

### **P_PERCENTAGE** (o foco principal)

Com o parâmetro `P_PERCENTAGE`, podes especificar a parcela de processos de diálogo de trabalho que são reservados e disponíveis apenas para o processamento de tarefas RFC assíncronas (aRFC).

Valores de 0 a 100 são possíveis, ou seja, que percentagem de processos utilizáveis são usados para paralelização.

Se nada for especificado, é usado um valor padrão de 50%.

**Como funciona o cálculo:**

O sistema limita o número de processos paralelos simultâneos calculando o máximo de tarefas paralelas permitido pelo sistema em todos os servidores de aplicação e reduz esse número de acordo com a percentagem. Por exemplo, se houver dois servidores de aplicação, cada um com 100 como contagem máxima de tarefas paralelas e P_PERCENTAGE = 75, a lógica interna limitará o número de processos paralelos simultâneos a 150.

### **P_LOCAL_SERVER**

Se definires o parâmetro P_LOCAL_SERVER com o valor 'X', apenas os processos do servidor atual são usados. Por padrão, os processos de diálogo de todos os servidores do sistema SAP atual são usados.

### **P_NUM_TASKS**

P_NUM_TASKS especifica um número fixo de processos que podem ser usados para paralelização.

### Outros parâmetros:
- **P_TIMEOUT** - Tempo limite para as tarefas
- **P_NUM_PROCESSES** - Número de processos
- **P_ABORT_ON_ERROR** - Se deve abortar em caso de erro
- **P_KEEPING_TASKS** - Se deve manter as tarefas

**Referência SAP:** https://github.com/SAP-docs/btp-cloud-platform/blob/main/docs/30-development/parallel-processing-1193647.md

---

## 3. Limitações e Thresholds de Segurança

### Processos Reservados pelo Sistema

Dois dos processos de diálogo livres num servidor são sempre retidos pelo sistema para solicitações de diálogo.

### Critérios para Disponibilização de Recursos

Se um dos seguintes critérios não for cumprido, nenhum recurso é disponibilizado para paralelização com RFC assíncrono (ou é acionada a exceção RESOURCE_FAILURE):

**Número de processos de diálogo disponíveis: 1**
- O valor padrão é 1. Por padrão, um certo número de processos de diálogo são mantidos livres como instâncias de reserva para outros fins, como logon no sistema ou programas de administração.

**Percentagem de processos de diálogo específicos do utilizador: 75%**
- Este valor padrão pode ser alterado usando o parâmetro de perfil `rdisp/rfc_max_own_used_wp`
- O número de processos de diálogo é definido usando o parâmetro `rdisp/wp_no_dia`

**Percentagem de solicitações em espera na fila de diálogo: 5%**
- Valor padrão (5% de todo o comprimento da fila de espera de diálogo)
- Pode ser alterado pelo parâmetro `rdisp/rfc_max_queue`
- O comprimento das filas de diálogo é definido usando `rdisp/elem_per_queue`

**Percentagem de logons no sistema: 90%**
- Se a percentagem de utilizadores já conectados exceder isso, nenhum recurso é liberado
- Pode ser alterado usando `rdisp/rfc_max_login`
- Máximo de logons configurado com `rdisp/tm_max_no`

**Percentagem de logons próprios no sistema: 25%**
- Valor padrão (25% de todos os logons)
- Pode ser alterado usando `rdisp/rfc_max_own_login`

**Percentagem de entradas de comunicação usadas: 90%**
- Limite padrão de 90%
- Pode ser alterado usando `rdisp/rfc_max_comm_entries`
- Máximo configurado com `rdisp/max_comm_entries`

**Referência SAP:** https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/abenapp_server_resources.htm

---

## 4. Como Consultar e Alterar Parâmetros de Perfil

### Transação RZ11 - Display Profile Parameter Values

A **transação RZ11** é usada para visualizar os valores atuais dos parâmetros de perfil do sistema.

**Como usar:**
1. Execute a transação **RZ11**
2. Introduz o nome do parâmetro (ex: `rdisp/rfc_max_own_used_wp`)
3. Clica em "Display" ou pressiona Enter
4. Visualiza o valor atual, valor padrão e descrição do parâmetro

**Parâmetros importantes para processamento paralelo:**
- `rdisp/rfc_max_own_used_wp` - Percentagem máxima de processos próprios
- `rdisp/wp_no_dia` - Número de processos de diálogo
- `rdisp/rfc_max_queue` - Percentagem máxima da fila
- `rdisp/elem_per_queue` - Elementos por fila
- `rdisp/rfc_max_login` - Percentagem máxima de logons
- `rdisp/tm_max_no` - Número máximo de logons
- `rdisp/rfc_max_own_login` - Percentagem de logons próprios
- `rdisp/rfc_max_comm_entries` - Percentagem de entradas de comunicação
- `rdisp/max_comm_entries` - Máximo de entradas de comunicação

### Transação RZ10 - Edit Profiles

A **transação RZ10** é usada para manutenção de perfis do sistema (requer autorização de Basis).

**Como usar:**
1. Execute a transação **RZ10**
2. Seleciona o perfil (geralmente DEFAULT ou perfil de instância)
3. Escolhe "Extended maintenance"
4. Clica em "Change"
5. Adiciona ou modifica parâmetros
6. Salva e ativa o perfil
7. **IMPORTANTE:** Requer reinício do sistema para os valores serem aplicados

### Transação RZ12 - RFC Destinations (Maintain)

Para configurações relacionadas a RFC:
- Execute **SM59** para manutenção de destinos RFC
- Execute **SMGW** para monitorização do Gateway

### Outras Transações Úteis

| Transação | Descrição | Uso |
|-----------|-----------|-----|
| **RZ11** | Display Profile Parameters | Consultar valores atuais dos parâmetros |
| **RZ10** | Edit Profiles | Alterar parâmetros de perfil (requer Basis) |
| **SM50** | Process Overview | Ver processos de trabalho do servidor atual |
| **SM51** | SAP Servers | Lista de servidores de aplicação |
| **SM66** | Global Work Process Overview | Visão global de processos em todos os servidores |
| **ST22** | ABAP Runtime Errors | Verificar dumps e erros de runtime |
| **SM59** | RFC Destinations | Configurar destinos RFC |
| **SMGW** | Gateway Monitor | Monitorizar o Gateway |
| **AL08** | Users Logged On | Ver utilizadores logados |
| **SM04** | User List | Lista de utilizadores no servidor atual |

### Exemplo de Consulta na RZ11

```
Transação: RZ11

Parâmetro: rdisp/rfc_max_own_used_wp
Valor atual: 75
Valor padrão: 75
Descrição: Maximum percentage of own used dialog WPs for aRFC
```

**⚠️ ATENÇÃO:** 
- Alterações em parâmetros de perfil devem ser feitas apenas pela equipa de Basis
- A maioria das alterações requer reinício do sistema
- Valores incorretos podem impactar severamente a performance do sistema
- Sempre consulta a equipa de Basis antes de solicitar alterações

---

## 5. Resposta à Pergunta Principal

### Cenário: 100 processos disponíveis, 30 em uso, p_percentage = 10

**P_PERCENTAGE refere-se à capacidade total configurada**, não aos processos livres no momento.

- Com `p_percentage = 10`, tens direito a **10% da capacidade total** = **10 processos**
- Estes 10 processos são a tua "quota" reservada
- **Se menos de 10 processos estiverem livres** (por exemplo, só 7 livres), terás de **esperar** até que processos sejam libertados
- O sistema sempre respeitará os thresholds de segurança mencionados acima

### Exemplo Prático:
```abap
NEW cl_abap_parallel( p_percentage = 10 )->run_inst(
  EXPORTING p_in_tab = it_processes
            p_debug = abap_false
  IMPORTING p_out_tab = DATA(lt_finished) ).
```

Neste caso, estás a requisitar 10% dos processos disponíveis para aRFC em todo o sistema.

---

## 6. Como Usar a Classe

### Método 1: Herdando de CL_ABAP_PARALLEL

```abap
CLASS zcl_my_parallel DEFINITION
  INHERITING FROM cl_abap_parallel.
  PUBLIC SECTION.
    METHODS do REDEFINITION.
ENDCLASS.

CLASS zcl_my_parallel IMPLEMENTATION.
  METHOD do.
    " Tua lógica aqui
    " p_in contém os dados de entrada (XSTRING)
    " p_out deve conter os dados de saída (XSTRING)
  ENDMETHOD.
ENDCLASS.
```

### Método 2: Implementando IF_ABAP_PARALLEL (Recomendado)

```abap
CLASS zcl_my_task DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_abap_parallel.
    METHODS constructor IMPORTING iv_data TYPE ...
    METHODS get_result RETURNING VALUE(rt_result) TYPE ...
ENDCLASS.

CLASS zcl_my_task IMPLEMENTATION.
  METHOD if_abap_parallel~do.
    " Tua lógica de processamento aqui
  ENDMETHOD.
ENDCLASS.
```

### Exemplo Completo de Uso

```abap
" Criar instâncias das tasks
DATA(lt_tasks) = VALUE cl_abap_parallel=>t_in_inst_tab(
  ( NEW zcl_my_task( iv_data = 'Data 1' ) )
  ( NEW zcl_my_task( iv_data = 'Data 2' ) )
  ( NEW zcl_my_task( iv_data = 'Data 3' ) )
).

" Executar em paralelo
NEW cl_abap_parallel( p_percentage = 10 )->run_inst(
  EXPORTING 
    p_in_tab  = lt_tasks
    p_debug   = abap_false
  IMPORTING 
    p_out_tab = DATA(lt_results)
).

" Processar resultados
LOOP AT lt_results ASSIGNING FIELD-SYMBOL(<result>).
  DATA(lo_task) = CAST zcl_my_task( <result>-inst ).
  DATA(result_data) = lo_task->get_result( ).
  " Processar result_data
ENDLOOP.
```

**Referência SAP:** https://software-heroes.com/en/blog/abap-cloud-parallel-processing

---

## 7. Considerações Importantes

Transportar os dados para um processo de diálogo livre pode levar algum tempo. Portanto, não é eficiente paralelizar operações de execução muito curta.

### Quando Usar Processamento Paralelo:
- ✅ Operações intensivas em dados
- ✅ Processamento de grandes volumes
- ✅ Cálculos complexos que podem ser divididos
- ✅ Quando há processos de diálogo disponíveis no sistema
- ✅ Operações que levam vários segundos ou minutos

### Quando NÃO Usar:
- ❌ Operações muito rápidas (overhead de transporte de dados)
- ❌ Sistemas com poucos processos de diálogo livres
- ❌ Operações que não podem ser paralelizadas
- ❌ Operações que levam menos de 1 segundo
- ❌ Quando há dependências entre as tasks

### Boas Práticas:
1. **Divida o trabalho de forma equilibrada** - Cada task deve ter carga similar
2. **Não exagere no p_percentage** - Deixe recursos para outros utilizadores
3. **Teste em ambiente de desenvolvimento primeiro** - Verifique o impacto no sistema
4. **Monitorize a execução** - Use SM66 para acompanhar
5. **Trate exceções adequadamente** - RESOURCE_FAILURE, COMMUNICATION_FAILURE, etc.
6. **Considere o overhead** - Para operações pequenas, o overhead pode ser maior que o benefício

**Referências SAP:**
- Documentação oficial: https://help.sap.com/docs/btp/sap-business-technology-platform/parallel-processing
- GitHub SAP BTP: https://github.com/SAP-docs/btp-cloud-platform/blob/main/docs/30-development/parallel-processing-1193647.md
- ABAP Cheat Sheets: https://github.com/SAP-samples/abap-cheat-sheets/blob/main/32_Performance_Notes.md#parallel-processing
- Community Blog: https://community.sap.com/t5/application-development-blog-posts/using-class-cl-abap-parallel-for-mass-parallel-dialog-work-processes/ba-p/13579844

---

## 8. Monitorização e Troubleshooting

### Transações para Monitorização em Tempo Real:

**SM50 - Process Overview (Servidor Atual)**
- Ver processos do servidor onde estás logado
- Identifica processos em "Running" ou "Wait"
- Mostra o utilizador, programa e tempo de execução

**SM66 - Global Work Process Overview**
- Visão consolidada de todos os servidores
- Essencial para monitorizar processamento paralelo distribuído
- Permite ver a carga em todos os application servers

**SM51 - SAP Servers**
- Lista todos os servidores de aplicação
- Mostra status e load de cada servidor
- Clica num servidor para saltar para o seu SM50

### Para Verificar o Comportamento do teu Processamento Paralelo:

1. **Antes de executar:** 
   - SM66 para ver carga atual do sistema
   - RZ11 para verificar parâmetros de RFC

2. **Durante a execução:**
   - SM66 para monitorizar os processos paralelos
   - Verifica a coluna "Task Name" para identificar as tuas tasks

3. **Em caso de problemas:**
   - ST22 para verificar runtime errors (especialmente RESOURCE_FAILURE)
   - SM21 para system log
   - ST05 para SQL trace (se suspeitas de problemas de performance)

### Exceções Comuns:

| Exceção | Causa | Solução |
|---------|-------|---------|
| **RESOURCE_FAILURE** | Nenhum processo de trabalho disponível | Reduzir p_percentage ou aguardar processos livres |
| **COMMUNICATION_FAILURE** | Problemas de comunicação RFC | Verificar configuração RFC (SM59) |
| **SYSTEM_FAILURE** | Falha no sistema remoto | Verificar logs do sistema (SM21) |
| **TIMEOUT** | Tempo limite excedido | Aumentar p_timeout ou otimizar código |

**Dica:** Se receberes RESOURCE_FAILURE frequentemente, pode significar:
- Sistema sobrecarregado
- P_PERCENTAGE demasiado alto
- Thresholds de segurança a bloquear recursos
- Necessidade de ajustar parâmetros com a equipa Basis

### Exemplo de Tratamento de Exceções:

```abap
TRY.
    NEW cl_abap_parallel( p_percentage = 10 )->run_inst(
      EXPORTING 
        p_in_tab  = lt_tasks
        p_debug   = abap_false
      IMPORTING 
        p_out_tab = DATA(lt_results)
    ).
    
  CATCH cx_root INTO DATA(lx_error).
    " Tratar erro
    DATA(lv_message) = lx_error->get_text( ).
    WRITE: / 'Erro no processamento paralelo:', lv_message.
ENDTRY.

" Verificar resultados individuais
LOOP AT lt_results ASSIGNING FIELD-SYMBOL(<result>).
  IF <result>-message IS NOT INITIAL.
    " Task falhou
    WRITE: / 'Task falhou:', <result>-message.
  ELSE.
    " Task teve sucesso
    DATA(lo_task) = CAST zcl_my_task( <result>-inst ).
    " Processar resultado
  ENDIF.
ENDLOOP.
```

---

## 9. Comparação: Processamento Sequencial vs Paralelo

### Exemplo de Cenário:

Processar 1000 registos, onde cada registo leva 0.5 segundos.

#### Processamento Sequencial:
```abap
DATA(lt_data) = get_data( ).  " 1000 registos

LOOP AT lt_data INTO DATA(ls_data).
  process_record( ls_data ).  " 0.5 segundos cada
ENDLOOP.

" Tempo total: 1000 * 0.5 = 500 segundos (≈8.3 minutos)
```

#### Processamento Paralelo (10 processos):
```abap
" Dividir em 10 grupos de 100 registos
DATA(lt_data) = get_data( ).  " 1000 registos
DATA(lt_tasks) = split_into_tasks( lt_data, 10 ).

NEW cl_abap_parallel( p_percentage = 10 )->run_inst(
  EXPORTING p_in_tab = lt_tasks
  IMPORTING p_out_tab = DATA(lt_results)
).

" Tempo total: 100 * 0.5 = 50 segundos (menos de 1 minuto)
" Ganho: 10x mais rápido!
```

### Gráfico de Performance:

```
Tempo de Execução vs Número de Processos Paralelos
|
|  500s ●━━━━━━━━━━━━━━━━━━━━━━━━━  (1 processo - sequencial)
|  250s      ●━━━━━━━━━━━━━━━━━━━  (2 processos)
|  125s           ●━━━━━━━━━━━━━━  (4 processos)
|   50s                     ●━━━━  (10 processos)
|   25s                          ● (20 processos - atenção ao overhead!)
|______|______|______|______|______|
     0     5    10    15    20    25
           Número de Processos
```

**Nota:** O ganho não é sempre linear devido ao overhead de comunicação e limitações de recursos.

---

## 10. Notas SAP Relevantes

### Notas Importantes:

- **3164040** - SSI_DISPATCH: SM50 and SM51 needed when called by CL_ABAP_PARALLEL
- Consulta SAP Notes sobre performance de RFC
- Verifica SAP Notes específicas para a tua versão do SAP_BASIS

### Como Consultar Notas SAP:

1. Acede ao SAP Support Portal: https://support.sap.com
2. Menu "SAP Notes"
3. Pesquisa por número ou palavras-chave (ex: "CL_ABAP_PARALLEL")

---

## 11. Checklist para Implementação

Antes de implementar processamento paralelo no teu código:

- [ ] Verifiquei que a operação leva tempo suficiente para justificar paralelização (>1 segundo)
- [ ] Dividi o trabalho de forma equilibrada entre tasks
- [ ] Testei em ambiente de desenvolvimento
- [ ] Verifiquei os parâmetros RFC do sistema (RZ11)
- [ ] Escolhi um p_percentage apropriado (recomendado: 5-15%)
- [ ] Implementei tratamento de exceções adequado
- [ ] Adicionei logging para monitorização
- [ ] Consultei a equipa de Basis se necessário
- [ ] Tenho plano B caso RESOURCE_FAILURE aconteça
- [ ] Documentei o código adequadamente

---

## 12. Recursos Adicionais

### Documentação Oficial SAP:
- [SAP Help - Parallel Processing](https://help.sap.com/docs/btp/sap-business-technology-platform/parallel-processing)
- [ABAP Keyword Documentation - RFC](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/abenrfc.htm)
- [RFC Thresholds](https://help.sap.com/doc/abapdocu_cp_index_htm/CLOUD/en-US/abenapp_server_resources.htm)

### GitHub e Exemplos:
- [SAP BTP Documentation](https://github.com/SAP-docs/btp-cloud-platform/blob/main/docs/30-development/parallel-processing-1193647.md)
- [ABAP Cheat Sheets](https://github.com/SAP-samples/abap-cheat-sheets/blob/main/32_Performance_Notes.md#parallel-processing)

### SAP Community:
- [Using CL_ABAP_PARALLEL for mass parallel dialog work processes](https://community.sap.com/t5/application-development-blog-posts/using-class-cl-abap-parallel-for-mass-parallel-dialog-work-processes/ba-p/13579844)
- [ABAP Cloud - Parallel Processing](https://software-heroes.com/en/blog/abap-cloud-parallel-processing)

---

## 13. FAQ - Perguntas Frequentes

### P: Posso usar CL_ABAP_PARALLEL em qualquer sistema SAP?
**R:** Sim, a classe está disponível desde versões mais antigas do SAP. Verifica a disponibilidade no teu sistema através da transação SE24.

### P: Qual é o valor ideal para p_percentage?
**R:** Depende do teu sistema, mas recomenda-se começar com valores baixos (5-15%) e ajustar conforme necessário. Nunca uses mais de 50% sem consultar a equipa de Basis.

### P: O que acontece se não houver processos disponíveis?
**R:** O sistema lança a exceção RESOURCE_FAILURE. Deves tratar esta exceção no teu código e ter um plano alternativo (ex: processar sequencialmente ou aguardar).

### P: Posso usar p_percentage e p_num_tasks ao mesmo tempo?
**R:** Sim, mas p_num_tasks tem precedência. Se especificares ambos, o sistema usará p_num_tasks.

### P: Como sei quantos processos de diálogo o meu sistema tem?
**R:** Usa a transação RZ11 e consulta o parâmetro `rdisp/wp_no_dia`.

### P: Processamento paralelo funciona em sistemas de desenvolvimento?
**R:** Sim, mas sistemas de desenvolvimento geralmente têm menos processos disponíveis. Testa primeiro em DEV, mas os resultados reais de performance serão melhores em PRD.

### P: Posso chamar uma função RFC dentro de uma task paralela?
**R:** Sim, mas tenha cuidado com aninhamento excessivo de RFCs, pois pode esgotar rapidamente os recursos disponíveis.

### P: O que é melhor: herdar de CL_ABAP_PARALLEL ou implementar IF_ABAP_PARALLEL?
**R:** Implementar IF_ABAP_PARALLEL é geralmente recomendado porque:
- Não precisas de serializar/deserializar dados (XSTRING)
- Código mais limpo e manutenível
- Melhor encapsulamento

---

## 14. Glossário

- **aRFC** - Asynchronous RFC (RFC Assíncrono)
- **Dialog Work Process** - Processo de trabalho de diálogo
- **Task** - Tarefa individual no processamento paralelo
- **Threshold** - Limite ou valor de segurança do sistema
- **Profile Parameter** - Parâmetro de configuração do sistema
- **RESOURCE_FAILURE** - Exceção quando não há recursos disponíveis
- **Application Server** - Servidor de aplicação SAP
- **Instance** - Instância do sistema SAP
- **Basis** - Equipa responsável pela administração técnica do SAP

---

**Documento criado em:** Outubro 2025  
**Versão:** 1.0  
**Autor:** Compilação de documentação oficial SAP

---

> 💡 **Dica Final:** Processamento paralelo é uma ferramenta poderosa, mas deve ser usada com responsabilidade. Sempre considera o impacto no sistema e nos outros utilizadores. Quando em dúvida, consulta a tua equipa de Basis!