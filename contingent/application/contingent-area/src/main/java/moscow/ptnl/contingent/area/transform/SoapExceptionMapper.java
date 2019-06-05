package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.area.error.ContingentException;
import moscow.ptnl.contingent.area.error.Validation;
import moscow.ptnl.contingent.area.error.ValidationMessage;
import moscow.ptnl.contingent.area.error.ValidationMessageType;
import moscow.ptnl.contingent.area.error.ValidationParameter;
import ru.mos.emias.contingent2.area.Fault;
import ru.mos.emias.system.v1.faults.BusinessFault;
import ru.mos.emias.system.v1.faults.ErrorMessage;
import ru.mos.emias.system.v1.faults.ErrorMessageCollection;
import ru.mos.emias.system.v1.faults.ErrorMessageTypes;
import ru.mos.emias.system.v1.faults.KeyValuePair;
import ru.mos.emias.system.v1.faults.UnexpectedFault;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class SoapExceptionMapper {

    public static Fault map(Exception e) {
        if (e instanceof ContingentException) {
            BusinessFault fault = new BusinessFault();
            fault.setType(fault.getType());
            fault.setMessages(map(((ContingentException) e).getValidation()));
            fault.setHasErrors(!((ContingentException) e).getValidation().isSuccess());

            return new Fault(e.getMessage(), fault);
        }
        UnexpectedFault fault = new UnexpectedFault();
        fault.setType(fault.getType());
        //Todo сделать вывод версии
        fault.setVersion("");
        StackTraceElement[] stackTrace = Thread.currentThread().getStackTrace();
        fault.setStackTrace(map(stackTrace));

        return new Fault((e.getMessage() != null ? e.getMessage() + " | " : "") + e.toString(), fault, e);
    }

    private static UnexpectedFault.StackTrace map(StackTraceElement[] stackTrace) {
        UnexpectedFault.StackTrace trace = new UnexpectedFault.StackTrace();

        if (stackTrace != null) {
            trace.getStackTraceRecords().addAll(Arrays.stream(stackTrace)
                    .limit(10)
                    .map(SoapExceptionMapper::map).collect(Collectors.toList()));
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
            messageCollection.getMessages().addAll(validation.getMessages().stream().map(SoapExceptionMapper::map).collect(Collectors.toList()));
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

    private static ErrorMessageTypes map(ValidationMessageType messageType) {
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
            newParameters.getParameters().addAll(parameters.stream().map(SoapExceptionMapper::map).collect(Collectors.toList()));

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
