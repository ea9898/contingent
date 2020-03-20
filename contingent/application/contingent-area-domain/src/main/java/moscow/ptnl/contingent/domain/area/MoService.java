package moscow.ptnl.contingent.domain.area;

import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;

import java.util.List;

public interface MoService {

    /**
     * (К_УУ_1) Добавление типов участков, доступных для МО
     * @param moId
     * @param areaTypeCodes
     * @throws ContingentException
     */
    void addMoAvailableAreaTypes(long moId, List<Long> areaTypeCodes) throws ContingentException;

    /**
     * (К_УУ_2)	Удаление типов участков из доступных для МО
     * @param moId
     * @param areaTypeCodes
     * @throws ContingentException
     */
    void delMoAvailableAreaTypes(long moId, List<Long> areaTypeCodes) throws ContingentException;

    /**
     * (К_УУ_3)	Предоставление типов участков, доступных для МО
     * @param moId
     * @return
     * @throws ContingentException
     */
    List<AreaType> getMoAvailableAreaTypes(long moId) throws ContingentException;
}
