<?php
namespace Petstore\Resources;

/**
 * @resource /pet "Operations about pets"
 * @basePath http://petstore.swagger.wordnik.com/api
 *
 * @consumes application/xml
 * @produces application/xml
 * @produces text/plain
 * @produces text/html
 * 
 */
class Pet
{
    /**
     * @api /pet/{petId}
     * @operation getPetById
     * @method GET
     * @summary "Find pet by ID"
     * @notes "Returns a pet based on ID"
     * @consumes application/json
     * @produces application/json
     * @return Pet
     * @response 400 "Invalid ID supplied"
     * @response 404 Tag "Pet not found"
     * @param petId:double[1.0 - 100000.0] #path "ID of pet that needs to be fetched"
     * @param auth:string #header "token used to authorization"
     * @auth/apiKey #header auth
     */
    public function getPetById() {

    }

    /**
     * @api /pet/findByStatus
     * @operation findPetsByStatus
     * @method GET
     * @summary "Finds Pets by status"
     * @notes "Multiple status values can be provided with comma seperated strings"
     * @return array[Pet]
     * @response 400 "Invalid status value"
     * @param status:string("available" | "pending" | "sold") #query "Status values that need to be considered for filter"
     */
    function findByStatus() {

    }

    /**
     * @api /pet/findByTags
     * @operation findPetsByTags
     * @method GET
     * @summary "Finds Pets by tags"
     * @notes "Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing."
     * @return array[Pet]
     * @response 400 "Invalid tag value"
     * @param tags:string #query "Tags to filter by"
     */
    function findPetsByTags();
}