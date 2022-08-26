Os dados representam os acumulados de chuva medidos pelas estações pluviométricas em instantes específicos.
Cada arquivo armazena registros de um ano específico. Os registros estão em ordem crescente de tempo.

As colunas disponíveis em cada arquivo são:
- tempo: data e hora do registro de chuva, no formato YYYY-mm-dd HH:MM:SS.
- id_estacao: identificador exclusivo da estação.
- nome_estacao_original: nome da estação. Há alguns casos de registros de estação com o mesmo nome, mas ids diferentes. Por isso, a coluna id_estacao foi incluída aqui junto com o nome da estação. O arquivo estacoes_niteroi.csv mostra isso.
- indice_pluv: índice pluviométrico medido no instante atual.
- quinzemin: índice pluviométrico acumulado nos últimos 15 minutos.
- trintamin: acumulado nos últimos 30 minutos.
- umahora: acumulado na última hora.
- seishoras: acumulado nas últimas 6 horas.
- dozehoras: acumulado nas últimas 12 horas.
- vintequatrohoras: acumulado nas últimas 24 horas.
- quarentaoitohoras: acumulado nas últimas 48 horas.
- setentaduashoras: acumulado nas últimas 72 horas.
- noventaseishoras: acumulado nas últimas 96 horas.
- mes: acumulado no último mês.
