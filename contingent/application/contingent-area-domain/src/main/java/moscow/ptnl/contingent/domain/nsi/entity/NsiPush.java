package moscow.ptnl.contingent.domain.nsi.entity;

public class NsiPush {

    private long id;
    private Object entity;
    private String action;

    public NsiPush(String action, long id) {
        this.action = action;
        this.id = id;
    }

    public Object getEntity() {
        return entity;
    }

    public void setEntity(Object entity) {
        this.entity = entity;
    }

    public String getAction() {
        return action;
    }

    public void setAction(String action) {
        this.action = action;
    }

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }
}