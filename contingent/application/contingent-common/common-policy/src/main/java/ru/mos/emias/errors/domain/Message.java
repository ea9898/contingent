// Decompiled by https://jdec.app/
package ru.mos.emias.errors.domain;

import java.util.Objects;

public class Message {
   String code;
   String message;

   public Message() {
   }

   public Message(String code, String message) {
      this.code = code;
      this.message = message;
   }

   public Message(ErrorReason errorReason) {
      this.code = errorReason.getCode();
      this.message = errorReason.getDescription();
   }

   public String getCode() {
      return this.code;
   }

   public void setCode(String code) {
      this.code = code;
   }

   public String getMessage() {
      return this.message;
   }

   public void setMessage(String message) {
      this.message = message;
   }

   public boolean isReasonOf(ErrorReason reason) {
      assert reason != null;

      return Objects.equals(this.code, reason.getCode());
   }

   @Override
   public boolean equals(Object o) {
      if (this == o) {
         return true;
      } else if (o != null && this.getClass() == o.getClass()) {
         Message message1 = (Message)o;
         return Objects.equals(this.code, message1.code) && Objects.equals(this.message, message1.message);
      } else {
         return false;
      }
   }

   @Override
   public int hashCode() {
      return Objects.hash(new Object[]{this.code, this.message});
   }

   @Override
   public String toString() {
      return String.format("%s:  %s", this.code, this.message);
   }
}