package com.goltd.agrigoussd.config.security;

import org.hibernate.event.spi.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class EncryptionListener implements PreInsertEventListener, PreUpdateEventListener, PostLoadEventListener {

    private FieldEncrypter fieldEncrypter;
    private FieldDecrypter fieldDecrypter;

    @Autowired
    public EncryptionListener(FieldEncrypter fieldEncrypter, FieldDecrypter fieldDecrypter) {
        this.fieldDecrypter = fieldDecrypter;
        this.fieldEncrypter = fieldEncrypter;
    }

    @Override
    public void onPostLoad(PostLoadEvent event) {
        fieldDecrypter.decrypt(event.getEntity());
    }

    @Override
    public boolean onPreInsert(PreInsertEvent event) {
        Object[] state = event.getState();
        String[] propertyNames = event.getPersister().getPropertyNames();
        Object entity = event.getEntity();
        fieldEncrypter.encrypt(state, propertyNames, entity);
        return false;
    }

    @Override
    public boolean onPreUpdate(PreUpdateEvent event) {
        Object[] state = event.getState();
        String[] propertyNames = event.getPersister().getPropertyNames();
        Object entity = event.getEntity();
        fieldEncrypter.encrypt(state, propertyNames, entity);
        return false;
    }
}
