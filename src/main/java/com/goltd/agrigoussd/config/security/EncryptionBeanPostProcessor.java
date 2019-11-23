package com.goltd.agrigoussd.config.security;

import org.hibernate.event.service.spi.EventListenerRegistry;
import org.hibernate.event.spi.EventType;
import org.hibernate.internal.SessionFactoryImpl;
import org.hibernate.jpa.HibernateEntityManagerFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.stereotype.Component;

import javax.persistence.EntityManagerFactory;

@Component
public class EncryptionBeanPostProcessor implements BeanPostProcessor {
    private static final Logger logger = LoggerFactory.getLogger(EncryptionBeanPostProcessor.class);


    private EncryptionListener encryptionListener;

    @Autowired
    public EncryptionBeanPostProcessor(EncryptionListener encryptionListener) {
        this.encryptionListener = encryptionListener;
    }

    @Override
    public Object postProcessBeforeInitialization(Object bean, String beanName) {
        try {
            return bean;
        } catch (BeansException ex) {
            logger.info("{}", ex);
            return ex.getMessage();
        }
    }

    @Override
    public Object postProcessAfterInitialization(Object bean, String beanName) {
        try {
            if (bean instanceof EntityManagerFactory) {
                HibernateEntityManagerFactory hibernateEntityManagerFactory = (HibernateEntityManagerFactory) bean;
                SessionFactoryImpl sessionFactoryImpl = (SessionFactoryImpl) hibernateEntityManagerFactory.getSessionFactory();
                EventListenerRegistry registry = sessionFactoryImpl.getServiceRegistry().getService(EventListenerRegistry.class);
                registry.appendListeners(EventType.POST_LOAD, encryptionListener);
                registry.appendListeners(EventType.PRE_INSERT, encryptionListener);
                registry.appendListeners(EventType.PRE_UPDATE, encryptionListener);
                logger.info("Encryption has been successfully set up");
            }
            return bean;
        } catch (BeansException ex) {
            logger.info("{}", ex);
            return ex.getMessage();
        }
    }
}
