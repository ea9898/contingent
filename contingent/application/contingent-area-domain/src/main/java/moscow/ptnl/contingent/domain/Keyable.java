/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.contingent.domain;

import java.io.Serializable;

/**
 *
 * @author m.kachalov
 */
public interface Keyable {
    
    /**
     * Возвращает уникальный ключ сущности.
     * 
     * @param <K>
     * @return 
     */
    <K extends Serializable> K getKey();
    
}
