package ru.mos.emias.errors.domain;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class OtherSecurityException extends Exception {
    
   private final List<Message> messages;

   public OtherSecurityException(Message message) {
      this.messages = new ArrayList();
      this.messages.add(message);
   }

   public OtherSecurityException(Message... messages) {
      this.messages = new ArrayList();
      Collections.addAll(this.messages, messages);
   }

   public OtherSecurityException(ErrorReason errorReason) {
      this(new Message(errorReason));
   }

   public List<Message> getMessages() {
      return this.messages;
   }

   @Override
   public String getMessage() {
      StringBuilder builder = new StringBuilder();
      this.messages.forEach((c) -> {
          if (c != null) {
            builder.append(c.toString()).append("\n");
          }
      });
      return builder.toString();
   }

   @Override
   public String getLocalizedMessage() {
      return this.getMessage();
   }
}
