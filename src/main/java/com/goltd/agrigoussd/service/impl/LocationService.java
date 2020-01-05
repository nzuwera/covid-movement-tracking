package com.goltd.agrigoussd.service.impl;

import com.goltd.agrigoussd.domain.Location;
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
    public List<Location> getlocationsByParentCode(String locationCode) {
        return locationRepository.getLocationsByParentIdCode(locationCode);
    }
}
