package com.goltd.agrigoussd.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.goltd.agrigoussd.helpers.enums.LocationType;
import org.hibernate.annotations.Type;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import java.math.BigInteger;
import java.util.List;
import java.util.UUID;


@Entity
@Table(name = "LOCATION",
        indexes = {
                @Index(name = "INDEX_LOCATION_CODE", columnList = "CODE", unique = true),
                @Index(name = "INDEX_LOCATION_LOCATION_TYPE", columnList = "TYPE", unique = false)
        },
        uniqueConstraints = {
                @UniqueConstraint(columnNames = "ID", name = "CONSTRAINT_LOCATION_ID"),
                @UniqueConstraint(columnNames = "CODE", name = "CONSTRAINT_LOCATION_CODE"),
                @UniqueConstraint(columnNames = {"NAME", "PARENT_ID"}, name = "CONSTRAINT_LOCATION_NAME_AND_PARENT_ID")
        })
public class Location {
    @NotNull
    @Id
    @Column(name = "ID", nullable = false)
    @Type(type = "pg-uuid")
    private UUID id;

    @Column(name = "STATE", columnDefinition = "integer not null")
    private Integer state;

    @Column(name = "VERSION", columnDefinition = "bigint not null")
    private BigInteger version;

    @Column(name = "CODE", columnDefinition = "character varying(40) NOT NULL")
    private String code;

    @Column(name = "DESCRIPTION", columnDefinition = "character varying(255)")
    private String description;

    @Column(name = "NAME", columnDefinition = "character varying(250) NOT NULL")
    private String name;

    @Column(name = "TYPE", columnDefinition = "character varying(31) NOT NULL")
    @Enumerated(EnumType.STRING)
    private LocationType type;

    @ManyToOne
    @JoinColumn(name = "PARENT_ID",referencedColumnName = "ID")
    private Location parentId;


    @OneToMany(mappedBy = "parentId", fetch = FetchType.LAZY)
    @JsonIgnore
    private List<Location> children;

    public Location() {
        // Empty constructor
    }

    public UUID getId() {
        return id;
    }

    public void setId(UUID id) {
        this.id = id;
    }

    public Integer getState() {
        return state;
    }

    public void setState(Integer state) {
        this.state = state;
    }

    public BigInteger getVersion() {
        return version;
    }

    public void setVersion(BigInteger version) {
        this.version = version;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public LocationType getType() {
        return type;
    }

    public void setType(LocationType type) {
        this.type = type;
    }

    public List<Location> getChildren() {
        return children;
    }

    public void setChildren(List<Location> children) {
        this.children = children;
    }

    @Override
    public String toString() {
        return "Location{" +
                "id=" + id +
                ", state=" + state +
                ", version=" + version +
                ", code='" + code + '\'' +
                ", description='" + description + '\'' +
                ", name='" + name + '\'' +
                ", type=" + type +
                ", children=" + children +
                '}';
    }
}
