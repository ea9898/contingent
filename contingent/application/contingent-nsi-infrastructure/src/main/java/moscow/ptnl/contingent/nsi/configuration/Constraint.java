/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.contingent.nsi.configuration;

/**
 *
 * @author mkachalov
 */
public interface Constraint {
    
    /**
     * Имя SI канала для приема сообщений NSI
     */
    String NSI_EVENT_CHANNEL_NAME = "NSIEventChannel";

    /**
     * Имя SI канала для приема сообщений NSI form
     */
    String NSI_FORM_CHANNEL_NAME = "NsiFormChannel";

    /**
     * Имя SI канала для приема сообщений NSI form
     */
    String NSI_FORM_REQUEST_CHANNEL_NAME = "NsiFormRequestChannel";
}
