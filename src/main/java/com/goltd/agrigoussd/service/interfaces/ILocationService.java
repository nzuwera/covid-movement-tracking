package com.goltd.agrigoussd.service.interfaces;

import com.goltd.agrigoussd.domain.Location;

import java.util.List;

public interface ILocationService {
    List<Location> getlocationsByParentCode(String locationCode);
}
