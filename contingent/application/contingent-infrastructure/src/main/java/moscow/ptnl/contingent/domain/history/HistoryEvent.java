/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.contingent.domain.history;

import java.io.Serializable;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 *
 * @author mkachalov
 */
@Entity
@Table(name = "JL_HISTORY")
public class HistoryEvent implements Serializable {
    
    @Id
    private long id;
    
    public HistoryEvent(){}
    
}
