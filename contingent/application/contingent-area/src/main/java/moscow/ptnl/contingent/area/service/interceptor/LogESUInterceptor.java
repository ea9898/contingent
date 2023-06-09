package moscow.ptnl.contingent.area.service.interceptor;

import moscow.ptnl.contingent.domain.area.EsuHelperService;
import moscow.ptnl.contingent.domain.area.heplers.AreaHelper;
import moscow.ptnl.contingent.domain.esu.EsuOutput;
import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import moscow.ptnl.contingent.domain.esu.event.annotation.LogESU;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.esu.event.AreaInfoEvent;
import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaRepository;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

/**
 * Перехватчик методов аннотированных как {@link LogESU}.
 * После того как метод завершит работу, производит отправку события в канал.
 * 
 * @author mkachalov
 */
@Aspect @Order(3)
@Component
public class LogESUInterceptor {
    
    private final static Logger LOG = LoggerFactory.getLogger(LogESUInterceptor.class);
    
    @Autowired
    private AreaHelper areaHelper;
    
    @Autowired
    private EsuHelperService esuHelperService;
    
    @Autowired
    private AreaRepository areaRepository;

    @Autowired
    private SettingService settingService;

    @Around(
        value = "execution(public * *(..)) && @annotation(annotation)",
        argNames="annotation"
    )
    public Object logESU(ProceedingJoinPoint joinPoint, moscow.ptnl.contingent.domain.esu.event.annotation.LogESU annotation) throws Throwable {
        
        MethodSignature signature = (MethodSignature) joinPoint.getSignature();
        Method method = signature.getMethod();
        Object[] args = joinPoint.getArgs();
            
        method.setAccessible(true);
        String methodName = annotation.methodName().isEmpty() ? method.getName() : annotation.methodName();
        
        final Object result = joinPoint.proceed();

        if (annotation.type().isAssignableFrom(AreaInfoEvent.class)) {
            if (Boolean.TRUE.equals(settingService.getPar4())) {
                List<Long> areaIds = getAreaId(annotation, method, methodName, args, result);

                if (areaIds.isEmpty()) {
                    throw new IllegalArgumentException("идентификатор сущности null");
                }
                
                areaRepository.getEntityManager().flush(); //актуализируем данные при не завершенной транзакции

                for (Long areaId : areaIds) {
                    Optional<Area> area = areaRepository.findById(areaId);

                    if (!area.isPresent()) {
                        throw new IllegalArgumentException("сущность с идентификатором " + areaId + " не найдена");
                    }
                    Area areaObject = area.get();
                    if (areaHelper.isAreaPrimary(areaObject)) {
                        esuHelperService.sendAreaInfoEvent(areaObject, methodName);
                    }
                }
            }
        } else {
            throw new RuntimeException("не поддерживаемый тип события");
        }

        return result;
    }
    
    private List<Long> getAreaId(LogESU annotation, Method method, String methodName, Object[] args, Object result) {
        Object value;

        if (annotation.useResult()) {
            if (result == null) {
                throw new IllegalArgumentException("в методе " + methodName + " результат не может быть null");
            }
            value = result;
        } else {
            if (annotation.parameters().length == 0) {
                throw new IllegalArgumentException("в методе " + methodName + " в списке параметров должно быть название параметра содержащего идентификатор сущности");
            }
            String parameterName = annotation.parameters()[0];
            Optional<Object> parameterValue = getParameterByName(method, args, parameterName);
            if (!parameterValue.isPresent()) {
                throw new IllegalArgumentException("в методе " +methodName + " не найден параметр " + parameterName);
            }
            value = parameterValue.get();

            if (value == null) {
                throw new IllegalArgumentException("в методе " + methodName + " параметр " + parameterName + " не может быть null");
            }
        }
        List<Long> areaIds = mapObjectToAreaIds(value);

        if (areaIds.isEmpty()) {
            throw new IllegalArgumentException("в методе " + methodName + " невозможно получить идентификатор сущности из результата выполнения метода");
        }
        return areaIds;
    }

    private List<Long> mapObjectToAreaIds(Object obj) {
        List<Long> result = new ArrayList<>();

        if (obj instanceof Long) {
            result.add((Long) obj);
        }
        else if (obj instanceof Area) {
            result.add(((Area) obj).getId());
        }
        else if (obj instanceof Collection) {
            ((Collection<?>) obj).forEach(o -> result.addAll(mapObjectToAreaIds(o)));
        }
        return result;
    }
    
    /**
     * Для определения имени аргумента при компиляции maven-compiler-plugin 
     * должен иметь настройку &lt;parameters&gt;true&lt;/parameters&gt;.
     * 
     * @param method
     * @param args
     * @param parameterName
     * @return 
     */
    private Optional<Object> getParameterByName(Method method, Object[] args, String parameterName) {
        for (int i = 0; i < method.getParameterCount(); i++) {            
            Parameter parameter = method.getParameters()[i];  
            LOG.debug("PARAMETER NAME: " + parameter.getName());
            if (parameter.getName().equals(parameterName)) {
                return Optional.of(args[i]);
            }
        }
        return Optional.of(args[0]);
    }
    
}
