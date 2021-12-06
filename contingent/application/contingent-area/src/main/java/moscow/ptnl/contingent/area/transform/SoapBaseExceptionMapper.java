package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.error.ValidationMessage;
import moscow.ptnl.contingent.error.ValidationParameter;
import moscow.ptnl.contingent.security.UserContextHolder;

import ru.mos.emias.errors.domain.ErrorMessageType;
import ru.mos.emias.errors.domain.Message;
import ru.mos.emias.errors.domain.OtherSecurityException;
import ru.mos.emias.errors.domain.UnauthorizedException;
import ru.mos.emias.system.v1.faults.BusinessFault;
import ru.mos.emias.system.v1.faults.ErrorMessage;
import ru.mos.emias.system.v1.faults.ErrorMessageCollection;
import ru.mos.emias.system.v1.faults.ErrorMessageTypes;
import ru.mos.emias.system.v1.faults.FaultTypes;
import ru.mos.emias.system.v1.faults.KeyValuePair;
import ru.mos.emias.system.v1.faults.SecurityExceptionTypes;
import ru.mos.emias.system.v1.faults.SecurityFault;
import ru.mos.emias.system.v1.faults.UnauthorizedRequestSecurityException;
import ru.mos.emias.system.v1.faults.UnexpectedFault;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public abstract class SoapBaseExceptionMapper<T extends Exception> {

    public T map(Exception e, UserContextMapper userContextMapper) {
        if (e instanceof ContingentException) {
            BusinessFault fault = new BusinessFault();
            fault.setType(fault.getType());
            fault.setMessages(map(((ContingentException) e).getValidation()));
            fault.setHasErrors(!((ContingentException) e).getValidation().isSuccess());

            return newFault(e.getMessage(), fault);
        } else if (e instanceof UnauthorizedException) {
            UnauthorizedRequestSecurityException.RequiredRights rights = new UnauthorizedRequestSecurityException.RequiredRights();
            rights.getUserRightIds().addAll(((UnauthorizedException) e).getUserRights());            
            
            UnauthorizedRequestSecurityException urse = new UnauthorizedRequestSecurityException();
            urse.setType(SecurityExceptionTypes.UNAUTHORIZED_REQUEST_EXCEPTION);
            urse.setRequiredRights(rights);
            
            SecurityFault fault = new SecurityFault();
            fault.setType(FaultTypes.SECURITY);
            fault.setUserContext(userContextMapper.entityToDtoTransform(UserContextHolder.getUserContext()));
            fault.setUnauthorizedRequestSecurityException(urse);

            return newFault(e.getMessage(), fault);
        } else if (e instanceof OtherSecurityException) {
            ru.mos.emias.system.v1.faults.OtherSecurityException ose = new ru.mos.emias.system.v1.faults.OtherSecurityException();
            List<Message> messages = ((OtherSecurityException) e).getMessages();
            if (!messages.isEmpty()) {
                ose.setMessage(map(messages.get(0)));
            }
            ose.setType(SecurityExceptionTypes.OTHER_SECURITY_EXCEPTION);
            
            SecurityFault fault = new SecurityFault();
            fault.setType(FaultTypes.SECURITY);
            fault.setUserContext(userContextMapper.entityToDtoTransform(UserContextHolder.getUserContext()));
            fault.setOtherSecurityException(ose);
            
            return newFault(e.getMessage(), fault);
        }
        UnexpectedFault fault = new UnexpectedFault();
        fault.setType(fault.getType());
        //Todo сделать вывод версии
        fault.setVersion("");
        StackTraceElement[] stackTrace = Thread.currentThread().getStackTrace();
        fault.setStackTrace(map(stackTrace));

        return newFault((e.getMessage() != null ? e.getMessage() + " | " : "") + e.toString(), fault, e);
    }

    public abstract T newFault(String message, ru.mos.emias.system.v1.faults.BaseFault fault);

    public abstract T newFault(String message, ru.mos.emias.system.v1.faults.BaseFault fault, java.lang.Throwable cause);
    
    public abstract T mapFault(ru.mos.emias.contingent2.area.Fault ex);
    
    public abstract T mapException(Exception ex);

    private static UnexpectedFault.StackTrace map(StackTraceElement[] stackTrace) {
        UnexpectedFault.StackTrace trace = new UnexpectedFault.StackTrace();

        if (stackTrace != null) {
            trace.getStackTraceRecords().addAll(Arrays.stream(stackTrace)
                    .limit(10)
                    .map(SoapBaseExceptionMapper::map).collect(Collectors.toList()));
        }
        return trace;
    }

    private static UnexpectedFault.StackTrace.StackTraceRecord map(StackTraceElement stackTraceElement) {
        UnexpectedFault.StackTrace.StackTraceRecord record = new UnexpectedFault.StackTrace.StackTraceRecord();
        record.setDeclaringClass(stackTraceElement.getClassName());
        record.setFileName(stackTraceElement.getFileName());
        record.setLineNumber(stackTraceElement.getLineNumber());
        record.setMethodName(stackTraceElement.getMethodName());

        return record;
    }

    private static ErrorMessageCollection map(Validation validation) {
        ErrorMessageCollection messageCollection = new ErrorMessageCollection();

        if (validation.getMessages() != null && !validation.getMessages().isEmpty()) {
            messageCollection.getMessages().addAll(validation.getMessages().stream().map(SoapBaseExceptionMapper::map).collect(Collectors.toList()));
        }
        return messageCollection;
    }

    private static ErrorMessage map(ValidationMessage validationMessage) {
        if (validationMessage != null) {
            ErrorMessage message = new ErrorMessage();
            message.setCode(validationMessage.getCode());
            message.setMessage(validationMessage.getMessage());
            message.setType(map(validationMessage.getType()));
            message.setParameters(map(validationMessage.getParameters()));

            return message;
        }
        return null;
    }
    
    private static ru.mos.emias.system.v1.faults.Message map(Message msg) {
        if (msg != null) {
            ru.mos.emias.system.v1.faults.Message message = new ru.mos.emias.system.v1.faults.Message();
            message.setCode(msg.getCode());
            message.setMessage(msg.getMessage());
            return message;
        }
        return null;
    }

    private static ErrorMessageTypes map(ErrorMessageType messageType) {
        switch (messageType) {
            case INFO: return ErrorMessageTypes.INFO;
            case WARNING: return ErrorMessageTypes.WARNING;
            case ERROR:
            default:
                return ErrorMessageTypes.ERROR;
        }
    }

    private static ErrorMessage.Parameters map(List<ValidationParameter> parameters) {
        if (parameters != null && !parameters.isEmpty()) {
            ErrorMessage.Parameters newParameters = new ErrorMessage.Parameters();
            newParameters.getParameters().addAll(parameters.stream().map(SoapBaseExceptionMapper::map).collect(Collectors.toList()));

            return newParameters;
        }
        return null;
    }

    private static KeyValuePair map(ValidationParameter parameter) {
        if (parameter != null) {
            KeyValuePair pair = new KeyValuePair();
            pair.setKey(parameter.getCode());
            pair.setValue(parameter.getValue());

            return pair;
        }
        return null;
    }
}
