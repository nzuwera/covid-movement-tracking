package com.goltd.agrigoussd.service.impl;

import com.goltd.agrigoussd.domain.UserAccount;
import com.goltd.agrigoussd.repository.UserRepository;
import com.goltd.agrigoussd.service.interfaces.IUserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
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
    public UserAccount create(UserAccount userAccount) {
        return userRepository.save(userAccount);
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
