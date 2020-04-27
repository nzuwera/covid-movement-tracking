package com.nzuwera.ussd.covidtracking.helpers;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UssdRequest {
    private String msisdn;
    private String sessionID;
    private String newrequest;
    private String input;

}
