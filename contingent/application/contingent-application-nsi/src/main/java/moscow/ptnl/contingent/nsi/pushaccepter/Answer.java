package moscow.ptnl.contingent.nsi.pushaccepter;

/**
 * Created by rudenko_ae on 03.09.2018.
 */
public class Answer {
    private boolean result;
    private String message;

    public Answer(boolean result, String message) {
        this.result = result;
        this.message = message;
    }

    public boolean isResult() {
        return result;
    }

    public void setResult(boolean result) {
        this.result = result;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }
}
