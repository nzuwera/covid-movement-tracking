package rw.centrika.ussd.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import rw.centrika.ussd.domain.UserAccount;
import rw.centrika.ussd.helpers.UTKit;
import rw.centrika.ussd.repository.UserRepository;
import rw.centrika.ussd.service.interfaces.IUserService;

import javax.transaction.Transactional;

@Service(value = "userService")
public class UserService implements IUserService {

    private UserRepository userRepository;

    @Autowired
    public UserService(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    /**
     * Create new userAccount
     *
     * @param userAccount userAccount object
     */
    @Override
    public void create(UserAccount userAccount) {
        userRepository.save(userAccount);
    }

    /**
     * Update userAccount
     *
     * @param userAccount userAccount object updated
     */
    @Override
    public void update(UserAccount userAccount) {
        userRepository.save(userAccount);
    }

    /**
     * Get UserAccount by phone number
     *
     * @param msisdn phone number
     * @return UserAccount object
     */
    @Override
    public UserAccount getUserByMsisdn(String msisdn) {
        return userRepository.findByMsisdn(msisdn);
    }

    /**
     * Check if user exists or not
     *
     * @param msisdn phone number
     * @return True or False
     */
    @Override
    public Boolean exists(String msisdn) {
        return userRepository.existsByMsisdn(msisdn);
    }

    /**
     * Delete User Account
     *
     * @param userAccount user account
     */
    @Override
    public void delete(UserAccount userAccount) {
        userRepository.delete(userAccount);
    }

    /**
     * Validate uses pin
     *
     * @param msisdn msisdn
     * @param pin    user pin
     * @return true | false
     */
    @Override
    public Boolean isValidPin(String msisdn, String pin) {
        UserAccount userAccount = userRepository.findByMsisdn(msisdn);
        return userAccount.getPin().equals(UTKit.securePassword(pin));
    }

    @Override
    @Transactional
    public void updatePin(String msisdn, String pin) {
        userRepository.updatePin(UTKit.securePassword(pin), msisdn);
    }
}
