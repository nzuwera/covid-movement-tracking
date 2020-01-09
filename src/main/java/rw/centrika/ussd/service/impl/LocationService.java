package rw.centrika.ussd.service.impl;

import rw.centrika.ussd.domain.Location;
import rw.centrika.ussd.repository.LocationRepository;
import rw.centrika.ussd.service.interfaces.ILocationService;
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
