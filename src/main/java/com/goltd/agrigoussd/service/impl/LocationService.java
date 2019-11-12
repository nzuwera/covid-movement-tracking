package com.goltd.agrigoussd.service.impl;

import com.goltd.agrigoussd.domain.Location;
import com.goltd.agrigoussd.helpers.enums.LocationType;
import com.goltd.agrigoussd.repository.LocationRepository;
import com.goltd.agrigoussd.service.interfaces.ILocationService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service()
public class LocationService implements ILocationService {

    private LocationRepository locationRepository;

    @Autowired
    public LocationService(LocationRepository locationRepository) {
        this.locationRepository = locationRepository;
    }

    @Override
    public List<Location> getProvinces() {
        return locationRepository.getProvinces();
    }

    @Override
    public List<Location> getDistricts(String provinceCode) {
        return locationRepository.getDistricts(provinceCode);
    }

    @Override
    public List<Location> getSectors(String districtCode) {
        return locationRepository.getSectors(districtCode);
    }

    @Override
    public List<Location> getCells(String sectorCode) {
        return locationRepository.getCells(sectorCode);
    }

    @Override
    public List<Location> getVillages(String cellCode) {
        return locationRepository.getVillages(cellCode);
    }

    @Override
    public List<Location> findLocationsByCodeLikeAndType(String locationCode, LocationType locationType) {
        return locationRepository.findLocationsByCodeLikeAndType(locationCode, locationType);
    }
}
