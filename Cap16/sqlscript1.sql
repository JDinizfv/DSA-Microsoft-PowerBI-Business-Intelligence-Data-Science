select round(avg(f.salario_hora)), e.estado
from "Cap16"."TB_FUNC" f, "Cap16"."TB_ENDERECO" e
where f.id = e.id_func
group by e.estado