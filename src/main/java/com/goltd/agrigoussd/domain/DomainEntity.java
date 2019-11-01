package com.goltd.agrigoussd.domain;


import org.hibernate.annotations.Type;

import javax.persistence.Column;
import javax.persistence.Id;
import javax.persistence.MappedSuperclass;
import javax.validation.constraints.NotNull;
import java.util.UUID;

@MappedSuperclass
public abstract class DomainEntity {


    @Id
    @Type(type = "pg-uuid")
    @NotNull
    @Column(name = "ID")
    private UUID id;

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }
}
