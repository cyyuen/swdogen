<?php
namespace Petstore\Resources;

/**
 * @resource /pet "Operations about pets"
 * @basePath http://petstore.swagger.wordnik.com/api
 * @produces application/json
 * @produces application/xml
 * @produces text/plain
 * @produces text/html
 */
class Pet
{
    /**
     * @url /pet/{petId}
     * @apiName getPetById
     * @method GET
     * @summary "Find pet by ID"
     * @notes "Returns a pet based on ID"
     * @return Pet
     * @response 400 "Invalid ID supplied"
     * @response 404 "Pet not found"
     * @param petId:double[1.0 - 100000.0] #path "ID of pet that needs to be fetched"
     */
    public function getPetById() {

    }

    /**
     * @url /pet/findByStatus
     * @apiName findPetsByStatus
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
     * @url /pet/findByTags
     * @apiName findPetsByTags
     * @method GET
     * @summary "Finds Pets by tags"
     * @notes "Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing."
     * @return array[Pet]
     * @response 400 "Invalid tag value"
     * @param tags:string #query "Tags to filter by"
     */
    function findPetsByTags();
}