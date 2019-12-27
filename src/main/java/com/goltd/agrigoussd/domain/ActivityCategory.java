package com.goltd.agrigoussd.domain;


import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name = "ACTIVITY_CATEGORY")
public class ActivityCategory extends AbstractEntity {
    private String name;

    public ActivityCategory() {
        //
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return "ActivityCategory{" +
                "name='" + name + '\'' +
                '}';
    }
}
