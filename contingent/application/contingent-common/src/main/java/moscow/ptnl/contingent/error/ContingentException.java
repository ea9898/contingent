package moscow.ptnl.contingent.error;

public class ContingentException extends Exception {

    private final Validation validation;

    public ContingentException(Validation validation) {
        this.validation = validation;
    }

    public ContingentException(ErrorReason errorReason, ValidationParameter... parameters) {
        this(new Validation().error(errorReason, parameters));
    }

    public Validation getValidation() {
        return validation;
    }

    @Override
    public String getMessage() {
        if (!this.validation.getMessages().isEmpty()) {
            return this.validation.getMessages().get(0).getMessage();
        }
        return super.getMessage();
    }
}