package rw.centrika.ussd.service;

import org.apache.commons.codec.language.Soundex;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import rw.centrika.ussd.domain.BusList;
import rw.centrika.ussd.domain.BusListRequest;
import rw.centrika.ussd.domain.BusStop;
import rw.centrika.ussd.domain.BusTime;
import rw.centrika.ussd.helpers.*;

import java.util.ArrayList;
import java.util.List;

@Service
public class BookingService {

    private static final Logger LOGGER = LoggerFactory.getLogger(BookingService.class);
    private static final String AUTH = "bearer 7yBp-NqiV1IedRfK1SxLRnF0-WboRuvvLVSGJW-8MgQCOXN7c9ORpkuSafEe9n4Bvo31Gc-zRxNPZzkCWLg1r5Ls8SIcUoseP3iZzpzVILyeRcwsLz0q5kBxUV4FElpnjbQ8Zg715R_3wCkDOfvphhJ32c8QZlPFgYeGHrSTxDsaZfSfqswUjnvvRWgNXrpMGOb_ZRsuAaJXSKvuL8SH70ZZ2LjuhViArtYvbFHl--MVCHZiVEO1EuWNAceh8QS8933rju2V4GigCiIluNmrPM_n_q8wUgvnSQfOntQk2pm-AzeLgh4tFUB0YUtw7qkr2q1WQHxljXtnwd8pqg5X_O6Id9lrc38HLsflL9KqAVl1XGAFd2zOlwmLbyOwSE-R3bF83OM3PQ6irWVGWyXw-TpatbzRibyWJxc2Rh44c8seBV7jLkIvf1XB2xQW9z6vseoKMHrjW_5fyiZT4Izr1hvbXO3qA7gd8iLusI4tjcQmHV-2qhz0I3xbe2j-DfVKn6E5Amito3xrq43ULsUOTb2E4Or6cZqBBfrlj7OVLIY-bGSGYLOvDm6-aR1con3YWdNkUKTwZslC1Lc-JOw46t3wcAbm425CrhVZM9CYDaoibdJNrlBcISh2P2Y3ZyuFTDuBRB_ZIteaAyDNzu3bevf7HqAEZ07BlfPB9jTKa91VV9FgzK-Z_1G1zl3W92ut";
    private static final String QUICK_BUS_GET_STOPS = "https://quickbus.centrika.rw/api/quickbus/QuickBusGetStops";
    private static final String QUICK_BUS_GET_LISTS = "http://quickbus.centrika.rw/api/quickbus/GetQuickBusSearchedList";
    private RestTemplate restTemplate;

    @Autowired
    public BookingService(RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
    }

    public BusStopSuccessResponse getBusStops() {
        HttpHeaders httpHeaders = new HttpHeaders();
        httpHeaders.add(Message.CONTENT_TYPE.string, String.valueOf(MediaType.APPLICATION_JSON));
        httpHeaders.add(Message.AUTHORIZATION.string, AUTH);

        HttpEntity httpEntity = new HttpEntity(httpHeaders);


        BusStopSuccessResponse successResponse;
        try {
            ResponseEntity<String> responseEntity = restTemplate.exchange(QUICK_BUS_GET_STOPS, HttpMethod.GET, httpEntity, String.class);

            String responseString = responseEntity.getBody();
            JSONObject jsonObject = UTKit.createJSONObject(responseString);
            JSONArray jsonArrayResult = (JSONArray) jsonObject.get("result");
            List<BusStop> busStops = new ArrayList<>();
            for (Object busStopJson : jsonArrayResult) {
                if (busStopJson instanceof JSONObject) {
                    busStops.add(new BusStop((JSONObject) busStopJson));
                }
            }
            String status = (String) jsonObject.get("status");
            String error = (String) jsonObject.get("error");
            successResponse = new BusStopSuccessResponse(busStops, status, error);
            LOGGER.info("successResponse {}", successResponse);
        } catch (Exception ex) {
            LOGGER.error("Error While getting busStop list {}", ex);
            successResponse = new BusStopSuccessResponse(new ArrayList<>(), "ERROR", "Error Occured while getting busStop list");
        }
        return successResponse;
    }

    public BusListSuccess getBusLists(BusListRequest listRequest) {
        BusListSuccess successResponse = new BusListSuccess();

        try {
            HttpHeaders httpHeaders = new HttpHeaders();
            httpHeaders.add(Message.CONTENT_TYPE.string, String.valueOf(MediaType.APPLICATION_JSON));
            httpHeaders.add(Message.AUTHORIZATION.string, AUTH);
            HttpEntity<BusListRequest> httpEntity = new HttpEntity<>(listRequest, httpHeaders);
            ResponseEntity<String> responseEntity = restTemplate.postForEntity(QUICK_BUS_GET_LISTS, httpEntity, String.class);

            String responseString = responseEntity.getBody();
            LOGGER.info("getBusList responseString {}", responseString);
            JSONObject jsonObject = UTKit.createJSONObject(responseString);
            JSONArray jsonArrayResult = (JSONArray) jsonObject.get("result");
            List<BusList> busLists = new ArrayList<>();
            for (Object busListJson : jsonArrayResult) {
                if (busListJson instanceof JSONObject) {
                    busLists.add(new BusList((JSONObject) busListJson));
                }
            }
            String status = (String) jsonObject.get("status");
            String error = (String) jsonObject.get("error");
            successResponse.setResult(busLists);
            successResponse.setStatus(status);
            successResponse.setError(error);
            LOGGER.info("successResponse {}", successResponse);
        } catch (Exception ex) {
            LOGGER.error("Error happened getting available bus list : {}", ex);
            successResponse.setResult(new ArrayList<>());
            successResponse.setStatus("ERROR");
            successResponse.setError("Error happened while getting available bus list");
        }
        LOGGER.info("getBusLists {}", successResponse);
        return successResponse;
    }

    public BusResponseObject showAvailableBuses(BusListSuccess busListSuccess) {
        BusResponseObject responseObject = new BusResponseObject();
        Boolean hasError = false;
        StringBuilder message = new StringBuilder();
        List<BusList> busLists = busListSuccess.getResult();
        if (!busLists.isEmpty()) {
            for (int i = 0; i < busLists.size(); i++) {
                message.append(i + 1);
                message.append(UTKit.DOT);
                message.append(UTKit.BLANK);
                message.append(busLists.get(i).getCompanyName());
                message.append(UTKit.BLANK);
                message.append(busLists.get(i).getName());
                message.append(UTKit.BLANK);
                message.append(busLists.get(i).getTotalAmount());
                message.append(busLists.get(i).getCurrency());
                message.append(UTKit.EOL);
            }
            responseObject.setMessage(message.toString());
        } else {
            hasError = true;
            responseObject.setMessage("No bus available on the selected time");
        }
        LOGGER.info("showAvailableBuses {}", message);
        responseObject.setStatus(hasError);
        return responseObject;
    }

    public BusResponseObject validateBusList(String input, BusListSuccess busListSuccess) {
        BusResponseObject responseObject = new BusResponseObject();
        Boolean hasError = false;
        List<BusList> busLists = busListSuccess.getResult();
        if (Boolean.TRUE.equals(UTKit.validateNumericalString(input))) {
            if (Integer.parseInt(input) <= busLists.size() && Integer.parseInt(input) > 0) {
                responseObject.setMessage(busLists.get(Integer.parseInt(input) - 1).getName() + UTKit.BLANK + busLists.get(Integer.parseInt(input) - 1).getTotalAmount());
            } else {
                hasError = true;
                responseObject.setMessage("Invalid bus");
            }
        } else {
            hasError = true;
            responseObject.setMessage("Invalid bus");
        }
        responseObject.setStatus(hasError);
        return responseObject;
    }

    public BusResponseObject getStopByName(String name) {
        BusResponseObject responseObject = new BusResponseObject();
        Boolean hasError = false;
        StringBuilder message = new StringBuilder();
        try {
            BusStopSuccessResponse successResponse = this.getBusStops();

            List<BusStop> busStopList = successResponse.getResult();
            List<BusStop> similarStops = new ArrayList<>();
            Soundex soundex = new Soundex();
            LOGGER.info("busStopList size {} list {}", busStopList.size(), busStopList);
            for (BusStop stop : busStopList) {
                if (soundex.encode(name).equals(soundex.encode(stop.getName()))) {
                    similarStops.add(stop);
                }
            }
            if (!similarStops.isEmpty()) {
                LOGGER.info("similarStops size {} list {}", similarStops.size(), similarStops);
                for (int i = 0; i < similarStops.size(); i++) {
                    message.append(i + 1);
                    message.append(UTKit.DOT);
                    message.append(UTKit.BLANK);
                    message.append(similarStops.get(i).getName());
                    message.append(UTKit.EOL);
                }
            } else {
                hasError = true;
                message.append(String.format(name, Message.NOT_FOUND.string));

            }
        } catch (Exception ex) {
            hasError = true;
            LOGGER.error("Error while getting location {}", ex.getMessage());
            message.append("Error while getting location");
        }
        responseObject.setStatus(hasError);
        responseObject.setMessage(message.toString());
        return responseObject;
    }

    public BusResponseObject validateSelectedBus(String input, String locationName) {
        BusResponseObject responseObject = new BusResponseObject();
        Boolean hasError = false;
        try {
            BusStopSuccessResponse successResponse = this.getBusStops();

            List<BusStop> busStopList = successResponse.getResult();
            List<BusStop> similarStops = new ArrayList<>();
            Soundex soundex = new Soundex();
            LOGGER.info("busStopList size {} list {}", busStopList.size(), busStopList);
            for (BusStop stop : busStopList) {
                if (soundex.encode(locationName).equals(soundex.encode(stop.getName()))) {
                    similarStops.add(stop);
                }
            }
            if (Integer.parseInt(input) > similarStops.size()) {
                hasError = true;
                responseObject.setMessage("Invalid location selected");
            } else {
                responseObject.setMessage(similarStops.get(Integer.parseInt(input) - 1).getName());
            }
        } catch (Exception ex) {
            hasError = true;
            LOGGER.error("Error validating selected location {}", ex.getMessage());
            responseObject.setMessage("Error validating selected location");
        }
        responseObject.setStatus(hasError);
        return responseObject;
    }


    public BusResponseObject validateBusList(String input, BusListRequest request) {
        BusResponseObject responseObject = new BusResponseObject();
        Boolean hasError = false;
        try {
            BusListSuccess successResponse = this.getBusLists(request);

            List<BusList> busLists = successResponse.getResult();
            LOGGER.info("busLists size {} list {}", busLists.size(), busLists);
            if (Integer.parseInt(input) > busLists.size() && Integer.parseInt(input) > 0) {
                hasError = true;
                responseObject.setMessage("Invalid bus selected");
            } else {
                responseObject.setMessage(busLists.get(Integer.parseInt(input) - 1).getName());
            }
        } catch (Exception ex) {
            hasError = true;
            LOGGER.error("Error validating selected bus {}", ex.getMessage());
            responseObject.setMessage("Error validating selected bus");
        }
        responseObject.setStatus(hasError);
        return responseObject;
    }

    public BusResponseObject validateDepartureTime(String input) {
        BusResponseObject responseObject = new BusResponseObject();
        Boolean hasError = false;
        List<BusTime> departureTimes = UTKit.getGetDepartureTime();
        if (Boolean.TRUE.equals(UTKit.validateNumericalString(input))) {
            if (Integer.parseInt(input) > departureTimes.size() && Integer.parseInt(input) > 0) {
                hasError = true;
                responseObject.setMessage("Invalid time selected");
            } else {
                String selectedTime = departureTimes.get(Integer.parseInt(input) - 1).getStartDate() + UTKit.BLANK + departureTimes.get(Integer.parseInt(input) - 1).getStartTime();
                responseObject.setMessage(selectedTime);
            }
        } else {
            hasError = true;
            responseObject.setMessage("Only numbers are allowed");
        }
        responseObject.setStatus(hasError);
        return responseObject;
    }

    public BusResponseObject validateBusCard(String input) {
        Boolean hasError = false;
        BusResponseObject responseObject = new BusResponseObject();
        if (Boolean.FALSE.equals(UTKit.validateSafariBusCardForm(input))) {
            hasError = true;
            responseObject.setMessage("Invalid card format CENTxxxxxxxxxxxx");
        } else {
            // Call API to validate card
            responseObject.setMessage("valide card format");
        }
        responseObject.setStatus(hasError);
        return responseObject;
    }
}
