package com.nzuwera.ussd.covidtracking.service;

import com.nzuwera.ussd.covidtracking.domain.*;
import com.nzuwera.ussd.covidtracking.helpers.*;
import org.apache.commons.codec.language.Soundex;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import rw.centrika.ussd.domain.*;
import rw.centrika.ussd.helpers.*;

import java.util.ArrayList;
import java.util.List;

@Service
public class BookingService {

    private static final Logger LOGGER = LoggerFactory.getLogger(BookingService.class);
    private static final String AUTH = "bearer 7yBp-NqiV1IedRfK1SxLRnF0-WboRuvvLVSGJW-8MgQCOXN7c9ORpkuSafEe9n4Bvo31Gc-zRxNPZzkCWLg1r5Ls8SIcUoseP3iZzpzVILyeRcwsLz0q5kBxUV4FElpnjbQ8Zg715R_3wCkDOfvphhJ32c8QZlPFgYeGHrSTxDsaZfSfqswUjnvvRWgNXrpMGOb_ZRsuAaJXSKvuL8SH70ZZ2LjuhViArtYvbFHl--MVCHZiVEO1EuWNAceh8QS8933rju2V4GigCiIluNmrPM_n_q8wUgvnSQfOntQk2pm-AzeLgh4tFUB0YUtw7qkr2q1WQHxljXtnwd8pqg5X_O6Id9lrc38HLsflL9KqAVl1XGAFd2zOlwmLbyOwSE-R3bF83OM3PQ6irWVGWyXw-TpatbzRibyWJxc2Rh44c8seBV7jLkIvf1XB2xQW9z6vseoKMHrjW_5fyiZT4Izr1hvbXO3qA7gd8iLusI4tjcQmHV-2qhz0I3xbe2j-DfVKn6E5Amito3xrq43ULsUOTb2E4Or6cZqBBfrlj7OVLIY-bGSGYLOvDm6-aR1con3YWdNkUKTwZslC1Lc-JOw46t3wcAbm425CrhVZM9CYDaoibdJNrlBcISh2P2Y3ZyuFTDuBRB_ZIteaAyDNzu3bevf7HqAEZ07BlfPB9jTKa91VV9FgzK-Z_1G1zl3W92ut";
    private static final String QUICK_BUS_GET_STOPS = "https://quickbus.centrika.rw/api/quickbus/QuickBusGetStops";
    private static final String QUICK_BUS_GET_LISTS = "http://quickbus.centrika.rw/api/quickbus/GetQuickBusSearchedList";
    private static final String QUICK_BUS_VALIDATE_CARD = "https://loyalty.centrika.rw/api/api/GetSafariBusCardBalance";
    private static final String QUICK_BUS_GET_SAFARI_BUS_TICKET = "https://loyalty.centrika.rw/api/api/GetSafariBusTicket";

    private RestTemplate restTemplate;

    @Autowired
    public BookingService(RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
    }

    public BookingService() {

    }

    /**
     * Get available bus name
     *
     * @return BusStopSuccessResponse
     */
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

    /**
     * Get Bus list from loyalty
     *
     * @param listRequest bus list request
     * @return BusListSuccess
     */
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

    /**
     * Show available buses
     *
     * @param busListSuccess BusListSuccess
     * @return BusResponseObject
     */
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

    /**
     * Validate bus list
     *
     * @param input          input
     * @param busListSuccess BusListSuccess
     * @return BusResponseObject
     */
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

    /**
     * GetStopByName
     *
     * @param name stop name
     * @return BusResponseObject
     */
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

    /**
     * Validate selected bus location
     *
     * @param input        user input
     * @param locationName bus stop
     * @return BusResponseObject
     */
    public BusResponseObject validateSelectedBusStop(String input, String locationName) {
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

    /**
     * Validate departure time
     *
     * @param input         current user input
     * @param departureDate departure date 1 or 2
     * @param timeOfTheDay  time of the day 1 ~ 6
     * @return Selected time dd/mm/yyyy hhHmm
     */
    public BusResponseObject validateDepartureTime(String input, String departureDate, String timeOfTheDay) {
        BusResponseObject responseObject = new BusResponseObject();
        Boolean hasError = false;
        List<BusTime> departureTimes = UTKit.getDepartureTime(departureDate, timeOfTheDay);
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

    /**
     * Validate bus card format and balance
     *
     * @param input card number
     * @return BusResponseObject
     */
    public BusResponseObject validateBusCard(String input, String amount) {
        LOGGER.info("tripFare {}", amount);
        CardValidationRequest cardValidationRequest = new CardValidationRequest();
        cardValidationRequest.setCardSerialNumber(input);


        Boolean hasError = false;
        String message = "";
        BusResponseObject responseObject = new BusResponseObject();
        if (Boolean.FALSE.equals(UTKit.validateSafariBusCardForm(input))) {
            hasError = true;
            responseObject.setMessage("Invalid card");
        } else {
            try {
                Double tripFare = Double.parseDouble(amount);
                CardValidationResponse validationResponse = getSafariBusCardBalance(cardValidationRequest);
                Double cardBalance = validationResponse.getResult().getBalance();
                if (cardBalance >= tripFare) {
                    message = "Success";
                } else {
                    hasError = true;
                    message = "Insufficient funds";
                }
            } catch (Exception ex) {
                hasError = true;
                message = "Card validation error";
                LOGGER.error("Card validation error {}", ex);
            }
        }
        LOGGER.info("validateBusCard validationResponse {}", message);
        responseObject.setStatus(hasError);
        responseObject.setMessage(message);
        return responseObject;
    }

    /**
     * GetSafariBusCardBalance
     *
     * @param cardValidationRequest validation request
     * @return validation response
     */
    public CardValidationResponse getSafariBusCardBalance(CardValidationRequest cardValidationRequest) {
        try {
            HttpHeaders httpHeaders = new HttpHeaders();
            httpHeaders.add(Message.CONTENT_TYPE.string, String.valueOf(MediaType.APPLICATION_JSON));
            httpHeaders.add(Message.AUTHORIZATION.string, AUTH);
            HttpEntity<CardValidationRequest> httpEntity = new HttpEntity<>(cardValidationRequest, httpHeaders);
            ResponseEntity<CardValidationResponse> responseEntity = restTemplate.postForEntity(QUICK_BUS_VALIDATE_CARD, httpEntity, CardValidationResponse.class);
            return responseEntity.getBody();
        } catch (Exception ex) {
            LOGGER.error("Call cardValidationApi error {}", ex);
            return new CardValidationResponse();
        }
    }

    /**
     * GetSafariBusTicket
     *
     * @param paymentRequest payment request
     * @return CardPaymentResponse
     */
    public CardPaymentResponse getSafariBusTicket(CardPaymentRequest paymentRequest) {
        try {
            HttpHeaders httpHeaders = new HttpHeaders();
            httpHeaders.add(Message.CONTENT_TYPE.string, String.valueOf(MediaType.APPLICATION_JSON));
            httpHeaders.add(Message.AUTHORIZATION.string, AUTH);
            HttpEntity<CardPaymentRequest> httpEntity = new HttpEntity<>(paymentRequest, httpHeaders);
            ResponseEntity<CardPaymentResponse> responseEntity = restTemplate.postForEntity(QUICK_BUS_GET_SAFARI_BUS_TICKET, httpEntity, CardPaymentResponse.class);
            return responseEntity.getBody();
        } catch (Exception ex) {
            LOGGER.error("Call cardPaymentApi error {}", ex);
            return new CardPaymentResponse();
        }
    }
}
