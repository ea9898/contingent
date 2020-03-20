package moscow.ptnl.contingent.sysop.service;

import moscow.ptnl.contingent.sysop.entity.Sysop;
import moscow.ptnl.contingent.error.ContingentException;

public interface SysopService {

    Sysop getOperationStatus(Long sysopId) throws ContingentException;
}
