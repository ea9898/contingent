package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.sysop.Sysop;
import moscow.ptnl.contingent.error.ContingentException;

public interface SysopService {

    Sysop getOperationStatus(Long sysopId) throws ContingentException;
}
