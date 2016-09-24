/* HMA FUNCTIONAL CODE */
;
(function() {

    /*
     * Subscribe email check 
     */
    var validateEmail = function(email) {
        var re = /^(([^<>()[\]\\.,;:\s@\"]+(\.[^<>()[\]\\.,;:\s@\"]+)*)|(\".+\"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
        return re.test(email);
    }

    var checkEmail = function(event) {

        if (validateEmail($('#mr-postman-email').val())) {
            return true;
        }
        else {
            $('#show-error-email').show();
            event.preventDefault();
            return false;
        }
    }
    /*
     * 
     */

    /*
     * Table update on user action (form submit, click on pagination) 
     */
    /* animation */
    var animation_on_ajax = function() {
        $('#listable tbody').fadeOut(500);
    }

    /* AJAX table render on form submit */
    var proxy_list_update_handler = function() {
        animation_on_ajax();
        $.ajax({
            type: "POST",
            dataType: "json",
            url: '/',
            data: $('#proxy-search-form').serialize(),
            success: function(data)
            {
                proxy_list_ajax_success_handler(data);
            }
        });
    }

    /* click on pagination link handler */
    var put_handler_on_pagination = function() {
        $('.pagination a').each(function() {
            /* for non-js */
            this.href = this.href + '#listable';
            var url = this.href;
            /* handler */
            $(this).on('click', function(event, url) {
                event.preventDefault();
                animation_on_ajax();
                var url = this.href;
                /* stop auto refreshing, and start if only we are on the first page */
                clearInterval(updateplInt);
                if($(this).text() == '1'){
                    startAutoRefsresh();
                }
                
                $.ajax({
                    type: "GET",
                    dataType: "json",
                    url: url,
                    success: function(data)
                    {
                        proxy_list_ajax_success_handler(data, url);
                    }
                });
                return false;
            })
        });
    }

    /* invoked after data from server is received */
    var proxy_list_ajax_success_handler = function(data, url) {
        /* replace table content */
        $('#listable tbody').html(data.table).fadeIn(100);
        /* replace new pagination */
        $('section.hma-pagination').html(data.pagination).fadeIn(100);
        put_handler_on_pagination();
        
        /* renew URL */
        if (history.pushState && (url || data.url)) {

            if (url)
            {
                var new_url = url;
            }
            else
            {
                var new_url = data.url + '#listable';
            }
            history.pushState('data', '', new_url);
        }
    }
    /*
     * 
     */

    /*
     * Auto refresh stuff
     */
    var updateplInt = false;
    var tsLast = 0;
    var sortSelector = 0;
    var sortParams = 0;
    /* time formatter */
    var elapsed = function(remaining)
    {
        var units = [1, 60, 3600];
        units = units.reverse();
        var unitNames = ['s', 'm', 'h'];
        unitNames = unitNames.reverse();

        var output = [];
        for (unit in units)
        {
            if (remaining >= units[unit])
            {
                var quotient = Math.floor(remaining / units[unit]);
                remaining = remaining % units[unit];
                output.push(quotient + unitNames[unit]);
            }
        }

        if (output.length > 2) {
            output.pop();
        }
        if (output.length > 1)
        {
            var last = output.pop();
            var finalstr = output.join(', ') + ' ' + last;
        } else {
            var finalstr = output.pop();
        }

        return finalstr;
    };
    
    /* find out current sort settings */
    var getSortBy = function()
    {
        var sortBy = $('input[name="sortBy"]').val();
        var orderBy = $('select[name="o"]').val();
        orderBy = orderBy == '0' ? 'desc' : 'asc';
      
        switch (sortBy)
        {
            case 'response_time':
                sortSelector = '.response_time';
                sortParams = {attr: 'rel', order: orderBy};

                return $('#listable tr:last .response_time').attr('rel');
                break;

            case 'connection_time':
                sortSelector = '.connection_time';
                sortParams = {attr: 'rel', order: orderBy};

                return $('#listable tr:last .connection_time').attr('rel');
                break;

            case 'country':
                sortSelector = '.country';
                sortParams = {attr: 'rel', order: orderBy}

                return $("#listable tr:last .country").attr('rel');
                break;

            case 'date':
                return '0';
                break;
        }

        return '';
    };

    var autorefresh_success_handler = function(data) {
        /* If error, exit and kill refresh */
        if (data.error)
        {
            clearInterval(updateplInt);
            return;
        }

        if (data.length == 0) {
            return;
        }


        /* Oust last *new* icons */
        if (data.entries.length > 0) {
            $('.newproxy').removeClass('newproxy');
        }

        /* Update timestamps we retrieve one from server
          so all follow same time */
        if (typeof(data.ts) != 'undefined')
        {
            tsLast = data.ts;
            $('.timestamp').each(function() {
                $(this).find('.updatets').each(function()
                {
                    $(this).html(elapsed(tsLast - $(this).parent().attr('rel')));
                });
            });
        }
        
        /* prepend the table with new entries */
        for (var i = 0; i < data.entries.length; i++)
        {
            /* Fade in new tr entry */
            $(data.entries[i]).fadeIn(i * 200).prependTo('#listable');

            /* Remove no results if exists */
            $('#noresult').remove();

            /* Remove last entry if at table max */
            if ($('#listable tr').size() >= parseInt($('#listable').attr('rel')))
            {
                $('#listable tr:last').remove();
            }
        }
        
        /* Reorder table */
        if (sortSelector !== 0)
        {
            $('#listable tr').tsort(sortSelector, sortParams);
        }
    };

    var autoRefsresh = function() {
        var per_page = $('#listable').attr('rel');
        var current_path = window.location.pathname;
        current_path = (current_path === '/')?'':current_path;
        $.ajax({
            type: "POST",
            dataType: "json",
            url: '/xhr/update' + current_path + '/'+per_page,
            data: {"q": getSortBy()},
            success: function(data)
            {
                autorefresh_success_handler(data);
            },
            error: function(jqXHR, textStatus, errorThrown){
                console.log('auto refresh error: '+  errorThrown);
                clearInterval(updateplInt);
            }        
        });
    };

    /* start auto-refres each 30 seconds */
    var startAutoRefsresh = function()
    {
        updateplInt = setInterval(function()
        {   
            autoRefsresh();
        }, 30000);
    };
    /*
     * 
     */

     var debug = function(){
         autoRefsresh();
     }


    /*
     *  put event handlers on the elements
     */
    $(document).ready(function()
    {
        $('#mr-postman-form').submit(function(event) {
            checkEmail(event);
        });

        /* ajax form submit */
        $('#proxy-list-upd-btn').on('click',
                function(event) {
                    event.preventDefault();
                    /* set new order (for auto-refresh) */
                    var sort_by_digit_to_word = ['date','response_time','connection_time','country'];
                    $('input[name="sortBy"]').val(sort_by_digit_to_word[$('select[name="s"]').val()]);
                    /* setup new perPagear */
                    var per_page_by_digit_to_value = [10,25,50,100];
                    $('#listable').attr('rel', per_page_by_digit_to_value[$('select[name="pp"]').val()])
                    proxy_list_update_handler();
                    return false;
                }
        );
        put_handler_on_pagination();

        /* form handlers */
        $('#ports').click(function()
        {
            $('#proxyAllPorts').prop('checked', '');
        });

        $('#country-sort-count').click(function()
        {
            $('#proxySelectedCountries option').tinysort({attr: 'rel', order: 'desc'});
        });

        $('#country-sort-name').click(function()
        {
            $('#proxySelectedCountries option').tinysort();
        });

        $('#proxyAllCountries').change(function()
        {
            if ($(this).is(':checked')) {
                $('#proxySelectedCountries option').prop('selected', true);
            } else {
                $('#proxySelectedCountries option').removeAttr('selected');
            }
        });

        $('#proxySelectedCountries').click(function()
        {
            $('#proxyAllCountries').prop('checked', '');
        });
        
        /* handlers for "protocols", "anonymity level", "speed", "connection time"
           at least one have to be checked, if all are unchecked - check all! */
        var group_of_checkboxes = ['pr', 'a', 'sp', 'ct'];
        for(i in group_of_checkboxes)
        {   
            eval('var '+group_of_checkboxes[i]+'_count=$("input[name=\''+group_of_checkboxes[i]+'[]\']:checked").length;$("input[name=\''+group_of_checkboxes[i]+'[]\']").change(function(){if($(this).is(":checked")){'+group_of_checkboxes[i]+'_count++}else{'+group_of_checkboxes[i]+'_count--;if('+group_of_checkboxes[i]+'_count===0){'+group_of_checkboxes[i]+'_count=$("input[name=\''+group_of_checkboxes[i]+'[]\']").prop("checked",true).length;}}});');
        }
        /* e.g. for pr (protocol)
        var pr_count = $("input[name='pr[]']:checked").length;
        $("input[name='pr[]']").change(function(){
            if($(this).is(':checked')){
               pr_count++;
            }else{
               pr_count--;
               if(pr_count === 0){  
                   pr_count = $("input[name='pr[]']").prop('checked', true).length;
               }
            }
        });
        */
       
        /* initiate auto-refresh (usually when we are on main page) */
        if($('#listable').data('auto-refresh') === 'on') {
            startAutoRefsresh();
        }
        
        /* set debug */
        $('#debug-starter').on('click', debug);

    });
})();
