package com.goltd.agrigoussd.service.impl;

import com.goltd.agrigoussd.domain.UserAccount;
import com.goltd.agrigoussd.repository.UserRepository;
import com.goltd.agrigoussd.service.interfaces.IUserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.UUID;

import static com.goltd.agrigoussd.helpers.enums.AccountState.PENDING_SUBSCRIPTION;

@Service
public class UserService implements IUserService {

    private UserRepository userRepository;

    @Autowired
    public UserService(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    /**
     * Create new user
     *
     * @param msisdn phone number
     */
    @Override
    public UserAccount create(String msisdn) {
        UserAccount newUserAccount = new UserAccount();
        newUserAccount.setId(UUID.randomUUID());
        newUserAccount.setMsisdn(msisdn);
        newUserAccount.setFullname(msisdn);
        newUserAccount.setPin(msisdn);
        newUserAccount.setAccountState(PENDING_SUBSCRIPTION);
        return userRepository.save(newUserAccount);
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
}
