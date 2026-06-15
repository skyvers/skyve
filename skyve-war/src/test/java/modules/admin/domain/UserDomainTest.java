package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Timestamp;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class UserDomainTest extends AbstractH2Test {

        @Test
        void dataBuilderCreatesUser() {
                User bean = new DataBuilder()
                                .fixture(FixtureType.crud)
                                .build(User.MODULE_NAME, User.DOCUMENT_NAME);
                assertNotNull(bean);
        }

        @Test
        void moduleAndDocumentNames() {
                User bean = User.newInstance();
                assertEquals("admin", bean.getBizModule());
                assertEquals("User", bean.getBizDocument());
        }

        @Test
        void passwordHashSetAndGet() {
                User bean = User.newInstance();
                bean.setPassword("hash123");
                assertEquals("hash123", bean.getPassword());
        }

        @Test
        void activeFlagSetAndGet() {
                User bean = User.newInstance();
                bean.setInactive(Boolean.TRUE);
                assertEquals(Boolean.TRUE, bean.getInactive());
                bean.setInactive(Boolean.FALSE);
                assertEquals(Boolean.FALSE, bean.getInactive());
        }

        @Test
        void wizardStateEnumValues() {
                assertNotNull(User.WizardState.values());
                assertEquals("confirmContact", User.WizardState.confirmContact.toCode());
                assertEquals("createContact", User.WizardState.createContact.toCode());
                assertEquals("confirmUserNameAndPassword", User.WizardState.confirmUserNameAndPassword.toCode());
                assertEquals("confirmGroupMemberships", User.WizardState.confirmGroupMemberships.toCode());
        }

        @Test
        void groupSelectionEnumValues() {
                assertNotNull(User.GroupSelection.values());
                assertEquals("existingGroups", User.GroupSelection.existingGroups.toCode());
                assertEquals("newGroup", User.GroupSelection.newGroup.toCode());
        }

        @Test
        void wizardStateFromCode() {
                assertEquals(User.WizardState.confirmContact, User.WizardState.fromCode("confirmContact"));
                assertEquals(User.WizardState.createContact, User.WizardState.fromCode("createContact"));
                assertNull(User.WizardState.fromCode("nonexistent"));
        }

        @Test
        void groupSelectionFromCode() {
                assertEquals(User.GroupSelection.existingGroups, User.GroupSelection.fromCode("existingGroups"));
                assertEquals(User.GroupSelection.newGroup, User.GroupSelection.fromCode("newGroup"));
                assertNull(User.GroupSelection.fromCode("nonexistent"));
        }

        @Test
        void wizardStateToDomainValues() {
                assertNotNull(User.WizardState.toDomainValues());
                assertFalse(User.WizardState.toDomainValues().isEmpty());
        }

        @Test
        void groupSelectionToDomainValues() {
                assertNotNull(User.GroupSelection.toDomainValues());
                assertFalse(User.GroupSelection.toDomainValues().isEmpty());
        }

        @Test
        void wizardStateSetAndGet() {
                User bean = User.newInstance();
                bean.setWizardState(User.WizardState.confirmContact);
                assertEquals(User.WizardState.confirmContact, bean.getWizardState());
        }

        @Test
        void conditionConfirmContactTrueWhenWizardStateMatches() {
                User bean = User.newInstance();
                bean.setWizardState(User.WizardState.confirmContact);
                assertTrue(bean.isConfirmContact());
                assertFalse(bean.isNotConfirmContact());
        }

        @Test
        void conditionConfirmContactFalseWhenOtherState() {
                User bean = User.newInstance();
                bean.setWizardState(User.WizardState.createContact);
                assertFalse(bean.isConfirmContact());
                assertTrue(bean.isNotConfirmContact());
        }

        @Test
        void conditionCreateContactTrueWhenWizardStateMatches() {
                User bean = User.newInstance();
                bean.setWizardState(User.WizardState.createContact);
                assertTrue(bean.isCreateContact());
                assertFalse(bean.isNotCreateContact());
        }

        @Test
        void conditionConfirmGroupMembershipsWhenWizardStateMatches() {
                User bean = User.newInstance();
                bean.setWizardState(User.WizardState.confirmGroupMemberships);
                assertTrue(bean.isConfirmGroupMemberships());
                assertFalse(bean.isNotConfirmGroupMemberships());
        }

        @Test
        void conditionConfirmUserNameAndPasswordWhenWizardStateMatches() {
                User bean = User.newInstance();
                bean.setWizardState(User.WizardState.confirmUserNameAndPassword);
                assertTrue(bean.isConfirmUserNameAndPassword());
                assertFalse(bean.isNotConfirmUserNameAndPassword());
        }

        @Test
        void conditionCandidateContactsEmptyInitially() {
                User bean = User.newInstance();
                assertTrue(bean.isCandidateContactsEmpty());
                assertFalse(bean.isNotCandidateContactsEmpty());
        }

        @Test
        void homeModuleSetAndGet() {
                User bean = User.newInstance();
                bean.setHomeModule("admin");
                assertEquals("admin", bean.getHomeModule());
        }

        @Test
        void generatedPasswordSetAndGet() {
                User bean = User.newInstance();
                bean.setGeneratedPassword("genpass123");
                assertEquals("genpass123", bean.getGeneratedPassword());
        }

        @Test
        void newPasswordSetAndGet() {
                User bean = User.newInstance();
                bean.setNewPassword("new123");
                assertEquals("new123", bean.getNewPassword());
        }

        @Test
        void confirmPasswordSetAndGet() {
                User bean = User.newInstance();
                bean.setConfirmPassword("confirm123");
                assertEquals("confirm123", bean.getConfirmPassword());
        }

        @Test
        void legacyIdSetAndGet() {
                User bean = User.newInstance();
                bean.setLegacyId("legacy-001");
                assertEquals("legacy-001", bean.getLegacyId());
        }

        @Test
        void passwordExpiredSetAndGet() {
                User bean = User.newInstance();
                bean.setPasswordExpired(Boolean.TRUE);
                assertEquals(Boolean.TRUE, bean.getPasswordExpired());
        }

        @Test
        void passwordLastChangedIPSetAndGet() {
                User bean = User.newInstance();
                bean.setPasswordLastChangedIP("192.168.1.1");
                assertEquals("192.168.1.1", bean.getPasswordLastChangedIP());
        }

        @Test
        void passwordLastChangedCountryCodeSetAndGet() {
                User bean = User.newInstance();
                bean.setPasswordLastChangedCountryCode("AU");
                assertEquals("AU", bean.getPasswordLastChangedCountryCode());
        }


        @Test
        void passwordResetTokenSetAndGet() {
                User bean = User.newInstance();
                bean.setPasswordResetToken("reset-token-abc");
                assertEquals("reset-token-abc", bean.getPasswordResetToken());
        }

        @Test
        void passwordHistorySetAndGet() {
                User bean = User.newInstance();
                bean.setPasswordHistory("old-hash-1|old-hash-2");
                assertEquals("old-hash-1|old-hash-2", bean.getPasswordHistory());
        }

        @Test
        void authenticationFailuresSetAndGet() {
                User bean = User.newInstance();
                bean.setAuthenticationFailures(Integer.valueOf(3));
                assertEquals(Integer.valueOf(3), bean.getAuthenticationFailures());
        }

        @Test
        void lastAuthenticationFailureSetAndGet() {
                User bean = User.newInstance();
                Timestamp now = new Timestamp();
                bean.setLastAuthenticationFailure(now);
                assertEquals(now, bean.getLastAuthenticationFailure());
        }

        @Test
        void groupSelectionSetAndGet() {
                User bean = User.newInstance();
                bean.setGroupSelection(User.GroupSelection.newGroup);
                assertEquals(User.GroupSelection.newGroup, bean.getGroupSelection());
        }

        @Test
        void groupsExistSetAndGet() {
                User bean = User.newInstance();
                bean.setGroupsExist(Boolean.TRUE);
                assertEquals(Boolean.TRUE, bean.getGroupsExist());
        }

        @Test
        void activatedSetAndGet() {
                User bean = User.newInstance();
                bean.setActivated(Boolean.TRUE);
                assertEquals(Boolean.TRUE, bean.getActivated());
        }

        @Test
        void activationCodeSetAndGet() {
                User bean = User.newInstance();
                bean.setActivationCode("ACT-123456");
                assertEquals("ACT-123456", bean.getActivationCode());
        }


        @Test
        void twoFactorCodeSetAndGet() {
                User bean = User.newInstance();
                bean.setTwoFactorCode("654321");
                assertEquals("654321", bean.getTwoFactorCode());
        }

        @Test
        void twoFactorTokenSetAndGet() {
                User bean = User.newInstance();
                bean.setTwoFactorToken("token-xyz");
                assertEquals("token-xyz", bean.getTwoFactorToken());
        }

        @Test
        void searchContactNameSetAndGet() {
                User bean = User.newInstance();
                bean.setSearchContactName("John");
                assertEquals("John", bean.getSearchContactName());
        }

        @Test
        void searchEmailSetAndGet() {
                User bean = User.newInstance();
                bean.setSearchEmail("john@example.com");
                assertEquals("john@example.com", bean.getSearchEmail());
        }

        @Test
        void contactSelectedSetAndGet() {
                User bean = User.newInstance();
                bean.setContactSelected(Boolean.TRUE);
                assertEquals(Boolean.TRUE, bean.getContactSelected());
        }

        @Test
        void conditionShowNextButtonTrueWhenCreateContact() {
                User bean = User.newInstance();
                bean.setWizardState(User.WizardState.createContact);
                assertTrue(bean.isShowNextButton());
                assertFalse(bean.isNotShowNextButton());
        }

        @Test
        void conditionShowNextButtonFalseWhenConfirmContact() {
                User bean = User.newInstance();
                bean.setWizardState(User.WizardState.confirmContact);
                assertFalse(bean.isShowNextButton());
                assertTrue(bean.isNotShowNextButton());
        }

        @Test
        void propertyNameConstants() {
                assertNotNull(User.passwordPropertyName);
                assertNotNull(User.homeModulePropertyName);
                assertNotNull(User.newPasswordPropertyName);
                assertNotNull(User.confirmPasswordPropertyName);
                assertNotNull(User.legacyIdPropertyName);
                assertNotNull(User.passwordExpiredPropertyName);
                assertNotNull(User.passwordLastChangedPropertyName);
                assertNotNull(User.passwordLastChangedIPPropertyName);
                assertNotNull(User.passwordLastChangedCountryCodePropertyName);
                assertNotNull(User.passwordLastChangedCountryNamePropertyName);
                assertNotNull(User.passwordResetTokenPropertyName);
                assertNotNull(User.passwordHistoryPropertyName);
                assertNotNull(User.authenticationFailuresPropertyName);
                assertNotNull(User.wizardStatePropertyName);
                assertNotNull(User.activationCodePropertyName);
                assertNotNull(User.twoFactorCodePropertyName);
                assertNotNull(User.twoFactorTokenPropertyName);
        }

        @Test
        void wizardStateToCode() {
                assertEquals("confirmContact", User.WizardState.confirmContact.toCode());
                assertEquals("createContact", User.WizardState.createContact.toCode());
                assertEquals("confirmUserNameAndPassword", User.WizardState.confirmUserNameAndPassword.toCode());
                assertEquals("confirmGroupMemberships", User.WizardState.confirmGroupMemberships.toCode());
        }

        @Test
        void wizardStateToLocalisedDescription() {
                assertNotNull(User.WizardState.confirmContact.toLocalisedDescription());
        }

        @Test
        void wizardStateToDomainValue() {
                assertNotNull(User.WizardState.confirmContact.toDomainValue());
        }

        @Test
        void wizardStateFromLocalisedDescription() {
                String desc = User.WizardState.confirmContact.toLocalisedDescription();
                assertEquals(User.WizardState.confirmContact, User.WizardState.fromLocalisedDescription(desc));
        }

        @Test
        void wizardStateFromLocalisedDescriptionUnknownReturnsNull() {
                assertNull(User.WizardState.fromLocalisedDescription("nonexistent state xyz"));
        }

        @Test
        void groupSelectionToCode() {
                assertEquals("existingGroups", User.GroupSelection.existingGroups.toCode());
                assertEquals("newGroup", User.GroupSelection.newGroup.toCode());
        }

        @Test
        void groupSelectionToLocalisedDescription() {
                assertNotNull(User.GroupSelection.existingGroups.toLocalisedDescription());
        }

        @Test
        void groupSelectionToDomainValue() {
                assertNotNull(User.GroupSelection.existingGroups.toDomainValue());
        }

        @Test
        void groupSelectionFromLocalisedDescription() {
                String desc = User.GroupSelection.existingGroups.toLocalisedDescription();
                assertEquals(User.GroupSelection.existingGroups, User.GroupSelection.fromLocalisedDescription(desc));
        }

        @Test
        void groupSelectionFromLocalisedDescriptionUnknownReturnsNull() {
                assertNull(User.GroupSelection.fromLocalisedDescription("nonexistent group xyz"));
        }

        @Test
        @SuppressWarnings("java:S4144")
        void isConfirmGroupMembershipsWhenStateSet() {
                User bean = User.newInstance();
                bean.setWizardState(User.WizardState.confirmGroupMemberships);
                assertTrue(bean.isConfirmGroupMemberships());
                assertFalse(bean.isNotConfirmGroupMemberships());
        }

        @Test
        @SuppressWarnings("java:S4144")
        void isConfirmUserNameAndPasswordWhenStateSet() {
                User bean = User.newInstance();
                bean.setWizardState(User.WizardState.confirmUserNameAndPassword);
                assertTrue(bean.isConfirmUserNameAndPassword());
                assertFalse(bean.isNotConfirmUserNameAndPassword());
        }

        @Test
        void isShowExistingGroupsWhenExistingGroupsSet() {
                User bean = User.newInstance();
                bean.setGroupSelection(User.GroupSelection.existingGroups);
                assertTrue(bean.isShowExistingGroups());
                assertFalse(bean.isNotShowExistingGroups());
        }

        @Test
        void isShowGroupCreatorWhenNewGroupSet() {
                User bean = User.newInstance();
                bean.setGroupSelection(User.GroupSelection.newGroup);
                assertTrue(bean.isShowGroupCreator());
                assertFalse(bean.isNotShowGroupCreator());
        }

        @Test
        @SuppressWarnings("java:S4144")
        void isCreateContactWhenStateSet() {
                User bean = User.newInstance();
                bean.setWizardState(User.WizardState.createContact);
                assertTrue(bean.isCreateContact());
                assertFalse(bean.isNotCreateContact());
        }

        @Test
        @SuppressWarnings("java:S4144")
        void isConfirmContactWhenStateSet() {
                User bean = User.newInstance();
                bean.setWizardState(User.WizardState.confirmContact);
                assertTrue(bean.isConfirmContact());
                assertFalse(bean.isNotConfirmContact());
        }

        @Test
        void isAccessDeniedConditions() {
                User bean = User.newInstance();
                assertFalse(bean.isAccessDenied());
                assertTrue(bean.isNotAccessDenied());
        }

        @Test
        @SuppressWarnings("boxing")
        void isDesignerConditions() {
                User bean = User.newInstance();
                assertEquals(!bean.isDesigner(), bean.isNotDesigner());
        }

        @Test
        void isInDataGroupConditions() {
                User bean = User.newInstance();
                assertFalse(bean.isInDataGroup());
                assertTrue(bean.isNotInDataGroup());
        }

        @Test
        void isOwningUserConditions() {
                User bean = User.newInstance();
                assertFalse(bean.isOwningUser());
                assertTrue(bean.isNotOwningUser());
        }

        @Test
        @SuppressWarnings("boxing")
        void isSecurityAdministratorConditions() {
                User bean = User.newInstance();
                assertEquals(!bean.isSecurityAdministrator(), bean.isNotSecurityAdministrator());
        }

        @Test
        void isSelfRegistrationEnabledConditions() {
                User bean = User.newInstance();
                assertFalse(bean.isSelfRegistrationEnabled());
                assertTrue(bean.isNotSelfRegistrationEnabled());
        }

        @Test
        void isCreatedConditions() {
                User bean = User.newInstance();
                assertFalse(bean.isCreated());
                assertTrue(bean.isNotCreated());
        }

        @Test
        void rolesAddAndGet() {
                User bean = User.newInstance();
                assertTrue(bean.getRoles().isEmpty());
                UserRole role = new UserRole();
                bean.addRolesElement(role);
                assertEquals(1, bean.getRoles().size());
                assertNotNull(bean.getRolesElementById(role.getBizId()));
        }

        @Test
        void rolesRemove() {
                User bean = User.newInstance();
                UserRole role = new UserRole();
                bean.addRolesElement(role);
                assertTrue(bean.removeRolesElement(role));
                assertTrue(bean.getRoles().isEmpty());
        }

        @Test
        void groupsAddAndGet() {
                User bean = User.newInstance();
                assertTrue(bean.getGroups().isEmpty());
                modules.admin.Group.GroupExtension group = new modules.admin.Group.GroupExtension();
                bean.addGroupsElement(group);
                assertEquals(1, bean.getGroups().size());
                assertNotNull(bean.getGroupsElementById(group.getBizId()));
        }

        @Test
        void groupsRemove() {
                User bean = User.newInstance();
                modules.admin.Group.GroupExtension group = new modules.admin.Group.GroupExtension();
                bean.addGroupsElement(group);
                assertTrue(bean.removeGroupsElement(group));
                assertTrue(bean.getGroups().isEmpty());
        }

        @Test
        void candidateContactsAddAndGet() {
                User bean = User.newInstance();
                assertTrue(bean.getCandidateContacts().isEmpty());
                UserCandidateContact candidate = new UserCandidateContact();
                bean.addCandidateContactsElement(candidate);
                assertEquals(1, bean.getCandidateContacts().size());
        }
}


