package com.goltd.agrigoussd.domain;

import org.hibernate.annotations.Type;

import javax.persistence.Column;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.MappedSuperclass;
import java.util.UUID;

@MappedSuperclass
public class AbstractEntity {

    @Id
    @GeneratedValue
    @Type(type = "pg-uuid")
    @Column(name = "ID", unique = true, length = 36)
    private UUID id;

    public AbstractEntity() {
        //
    }

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }
}
