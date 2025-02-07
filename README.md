# ABAP Parallel Material Processing

![Static Badge](https://img.shields.io/badge/Development-ABAP-blue)
![ABAP Version](https://img.shields.io/badge/ABAP-7.4%2B-brightgreen)
![License](https://img.shields.io/badge/License-MIT-blue)
![Build Status](https://img.shields.io/badge/Build-Passing-success)

<!-- ![N|Solid](img/sap-abap.jpeg) -->

![GitHub commit activity (branch)](https://img.shields.io/github/commit-activity/t/edmilson-nascimento/abap_parallel)
![Static Badge](https://img.shields.io/badge/gabriel_alencar-abap-orange)
![Static Badge](https://img.shields.io/badge/poo-abap-teal)
![Static Badge](https://img.shields.io/badge/murilo_borges-abap-lime)


ABAP implementation for parallel processing of material data using `CL_ABAP_PARALLEL`. This solution demonstrates efficient batch processing of material master data with SAP BAPI integration.

## Features

- 🚀 Parallel data retrieval using ABAP Parallel Processing Framework
- 🔄 BAPI integration for material data operation (`BAPI_MATERIAL_GET_DETAIL`)
- ⚡ Configurable resource allocation (30% system resources in example)
- 📊 Batch processing of up to 30,000 materials
- 🛠️ Error handling with BAPI return messages
- 📈 Scalable architecture for enterprise-level material processing

## Installation

1. **Clone Repository**
   ```bash
   git clone https://github.com/edmilson-nascimento/abap_parallel.git
   ```
2. **Import to SAP System**
   - Use [abapGit](https://abapgit.org/) to import the repository
   - Activate all objects via SE80

## Usage

### Execute Report
Run transaction `SE38` and execute program `ZPARALLEL_MATERIAL_PROCESSING`

### Process Flow
```mermaid
graph TD
    A[Main Process] --> B[Get Material List]
    B --> C[Split into Parallel Tasks]
    C --> D[Task 1: Material Get Detail]
    C --> E[Task 2: Material Get Detail]
    C --> F[Task N: Material Get Detail]
    D --> G[Aggregate Results]
    E --> G
    F --> G
    G --> H[Display Results]
```

### Key Methods
```abap
" Main execution method
CLASS main IMPLEMENTATION.
  METHOD process.
    " Parallel processing setup
    DATA(parallel) = NEW single_task( p_percentage = 30 ).
    parallel->run( ... )
  ENDMETHOD.
ENDCLASS.
```

## Configuration

### Resource Allocation
Adjust parallel processing resources in `main=>process`:
```abap
DATA(parallel) = 
  NEW single_task( p_percentage = 30 ) " 30% system resources
```

### Processing Modes
| Mode | Description              |
|------|--------------------------|
| 2    | Read Material Details    |

## Contributing

Contributions are welcome! Please follow these steps:
1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## License

Distributed under the MIT License. See `LICENSE` for more information.

<!-- ## Pontos de atenção 📝
- Quase ninguém fez isso, então, claro que nos (eu e ~~as vozes~~ os algoritmos na minha cabeça) vamos fazer -->

