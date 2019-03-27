/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.util;

/**
 *
 * @author m.kachalov
 */
public class Strings {
    
    private Strings(){}
    
    public static boolean isNullOrEmpty(String source) {
        return source == null || source.isEmpty();
    }
    
}
