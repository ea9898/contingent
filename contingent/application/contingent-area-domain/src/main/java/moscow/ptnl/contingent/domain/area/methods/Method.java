package moscow.ptnl.contingent.domain.area.methods;

import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.Validation;

public abstract class Method<TParams, TResult> {

    protected final Validation validation = new Validation();

    protected final TParams params;

    public Method(TParams params) {
        this.params = params;
    }

    public abstract Validation validate();

    public abstract TResult execute() throws ContingentException;
}
