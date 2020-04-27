package com.nzuwera.ussd.covidtracking.domain;

import com.nzuwera.ussd.covidtracking.helpers.enums.Question;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import javax.validation.constraints.NotNull;

@Entity
@Table(name = "USSD_MENU")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UssdMenu extends AbstractEntity {

    @Column(name = "TITLE_ENG")
    private String titleEng;

    @Column(name = "TITLE_KIN")
    private String titleKin;

    @NotNull
    @Column(name = "QUESTION", nullable = false, columnDefinition = "varchar(50) default 'LOGIN'")
    @Enumerated(EnumType.STRING)
    private Question question;

    @Column(name = "IS_LEAF", nullable = false, columnDefinition = "BOOLEAN default FALSE")
    private Boolean isLeaf;

    @Column(name = "PRIORITY")
    private int priority;

    @ManyToOne()
    @JoinColumn(name = "PARENT_ID",referencedColumnName = "ID")
    private UssdMenu parentMenu;

}
