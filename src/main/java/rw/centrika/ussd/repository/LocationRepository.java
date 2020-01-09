package rw.centrika.ussd.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import rw.centrika.ussd.domain.Location;
import rw.centrika.ussd.helpers.enums.LocationType;

import java.util.List;
import java.util.UUID;

@Repository
public interface LocationRepository extends JpaRepository<Location, UUID> {

    @Query(value = "select l from Location l where l.type = 'PROVINCE'")
    List<Location> getProvinces();

    @Query(value = "select l from Location l where l.code like CONCAT(:provinceCode,'%') and l.type =  'DISTRICT'")
    List<Location> getDistricts(@Param("provinceCode") String provinceCode);

    @Query(value = "select l from Location l where l.code like CONCAT(:districtCode,'%') and l.type =  'SECTOR'")
    List<Location> getSectors(@Param("districtCode") String districtCode);

    @Query(value = "select l from Location l where l.code like CONCAT(:sectorCode,'%') and l.type =  'CELL'")
    List<Location> getCells(@Param("sectorCode") String sectorCode);

    @Query(value = "select l from Location l where l.code like CONCAT(:cellCode,'%') and l.type =  'VILLAGE'")
    List<Location> getVillages(@Param("cellCode") String cellCode);

    @Query(value = "select l from Location l where l.code = :locationCode and l.type = :locationType")
    List<Location> findLocationsByCodeLikeAndType(@Param("locationCode") String locationCode, @Param("locationType") LocationType locationType);

    @Query(value = "select s from Location l inner join Location s on l.id = s.parentId.id where l.code = :locationCode order by s.code")
    List<Location> getLocationsByParentIdCode(@Param("locationCode") String locationCode);


}