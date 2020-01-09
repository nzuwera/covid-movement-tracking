package rw.centrika.ussd.service.interfaces;

import rw.centrika.ussd.domain.Location;

import java.util.List;

public interface ILocationService {
    List<Location> getlocationsByParentCode(String locationCode);
}
