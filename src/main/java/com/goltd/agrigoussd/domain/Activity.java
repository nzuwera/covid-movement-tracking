package com.goltd.agrigoussd.domain;


import javax.persistence.*;
import javax.validation.constraints.NotNull;

@Entity
@Table(name = "ACTIVITY")
public class Activity extends AbstractEntity {
    @NotNull
    @Column
    private String name;

    @ManyToOne
    @JoinColumn
    private ActivityCategory category;

    public Activity() {
        //
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public ActivityCategory getCategory() {
        return category;
    }

    public void setCategory(ActivityCategory category) {
        this.category = category;
    }

    @Override
    public String toString() {
        return "Activity{" +
                "name='" + name + '\'' +
                ", category=" + category +
                '}';
    }
}
