package com.goltd.agrigoussd.service.interfaces;

import com.goltd.agrigoussd.domain.Location;
import com.goltd.agrigoussd.helpers.enums.LocationType;

import java.util.List;

public interface ILocationService {
    List<Location> getProvinces();
    List<Location> getDistricts(String provinceCode);
    List<Location> getSectors(String districtCode);
    List<Location> getCells(String sectorCode);
    List<Location> getVillages(String cellCode);
    List<Location> findLocationsByCodeLikeAndType(String locationCode, LocationType locationType);
}
