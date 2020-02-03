package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.sysop.Sysop;
import moscow.ptnl.contingent.area.error.SysopErrorReason;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.error.ValidationParameter;
import moscow.ptnl.contingent.repository.sysop.SysopCRUDRepository;

import moscow.ptnl.contingent.repository.sysop.SysopMsgCRUDRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class SysopServiceImpl implements SysopService {

    private final static Logger LOG = LoggerFactory.getLogger(SysopServiceImpl.class);

    @Autowired
    private SysopCRUDRepository sysopCRUDRepository;

    // (К_ОС_1) Получение статуса выполнения операции
    @Override
    public Sysop getOperationStatus(Long sysopId) throws ContingentException {
        Validation validation = new Validation();
        // TODO implement 1.
        // 2.
        Sysop sysop = sysopCRUDRepository.findById(sysopId).orElseGet(() -> {
            validation.error(SysopErrorReason.OPERATION_NOT_FOUND, new ValidationParameter("sysopId", sysopId));
            return null;
        });
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        // 3. 4. 5.
        return sysop;
    }
}
