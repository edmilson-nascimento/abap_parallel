# ABAP parallel
 
![N|Solid](img/sap-abap.jpeg)

![Static Badge](https://img.shields.io/badge/development-abap-blue)
![GitHub commit activity (branch)](https://img.shields.io/github/commit-activity/t/edmilson-nascimento/abap_parallel)
![Static Badge](https://img.shields.io/badge/gabriel_alencar-abap-orange)
![Static Badge](https://img.shields.io/badge/poo-abap-teal)
![Static Badge](https://img.shields.io/badge/murilo_borges-abap-lime)


 Paralelismo.

> 🗘 Este documento, assim como o negócio, está em constante fase de melhoria e adaptação.

 Usando a interface standard SAP `cl_abap_parallel` para fazer processamento em paralelismo. A interface de base será 
 Um dos links de referencia usados é [esse](https://sascha-dev.de/sap-entwicklung/parallelverarbeitung-unter-abap-mit-der-klasse-cl_abap_parallel/).



 ~~no momento, eu penso que seja uma boa ideia fazer disso um post no SAP Blogs, mas essa animação vai por agua em alguns dias~~


## Glossário

| Sigla | Significado | Descrição |
|-----|-----------|------------|
| BC |Business Consulting | ~~Find Clarity in Chaos~~ ABAP, Desenvolvedor SAP, Consultor ABAP, SAP DEV|
 FM | Function Module ||

## Fluxo da solução

```mermaid
%%{ init: { 'flowchart': { 'curve': 'basis' } } }%%
flowchart TB

    Begin((" ")):::startClass --> service-now([Processamento])
    service-now --> Atendimento-BC(["Lista de itens"])
    Atendimento-BC --> Q1{" "}

    Q1 -- Sim --> Quermesse("Adic. na fila") 
            --> End
    Q1 -- Não -->

End(((" "))):::endClass
```

### Processamento
```abap

write:/ 'Hello parallel' .

```

## Pontos de atenção 📝

- Quase ninguém fez isso, então, claro que nos (eu e ~~as vozes~~ os algoritmos na minha cabeça) vamos fazer
- 
