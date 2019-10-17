/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.contingent.domain.security.setting;

import java.util.HashMap;
import java.util.Map;
import javax.xml.bind.annotation.XmlRootElement;

/**
 *
 * @author mkachalov
 */
@XmlRootElement
public class AuthService {
    
    private boolean enabled = true;
    private Map<String, AuthMethod> authMethods = new HashMap<>();

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    public Map<String, AuthMethod> getAuthMethods() {
        return authMethods;
    }

    public void setAuthMethods(Map<String, AuthMethod> authMethods) {
        this.authMethods = authMethods;
    }
    
}
