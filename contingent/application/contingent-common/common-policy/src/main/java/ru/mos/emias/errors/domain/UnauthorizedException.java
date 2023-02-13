package ru.mos.emias.errors.domain;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

public class UnauthorizedException extends Exception {
    
   private final List<Long> userRights = new ArrayList();

   public UnauthorizedException(long... userRights) {
      if (userRights != null) {
         this.userRights.addAll((Collection)Arrays.stream(userRights).boxed().collect(Collectors.toList()));
      }

   }

   public List<Long> getUserRights() {
      return this.userRights;
   }

    @Override
    public String getMessage() {
        StringBuilder builder = new StringBuilder();
        builder.append("Unauthorized exception: ");
        builder.append(String.join(", ", (Iterable)this.userRights.stream().filter((c) -> c != null).map((c) -> c.toString()).collect(Collectors.toList())));
        return builder.toString();
    }

    @Override
    public String getLocalizedMessage() {
       return this.getMessage();
    }
}
